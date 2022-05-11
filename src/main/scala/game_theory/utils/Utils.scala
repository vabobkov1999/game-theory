package game_theory.utils

import game_theory.{Coalition, MSA}
import org.apache.poi.hssf.usermodel.HSSFWorkbook
import org.apache.poi.ss.usermodel.{DataFormatter, Row}
import org.apache.poi.xssf.usermodel.XSSFWorkbook

import java.io.{File, FileInputStream, IOException}
import java.nio.file.{FileVisitResult, Files, Path, Paths, SimpleFileVisitor}
import java.nio.file.attribute.BasicFileAttributes
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import scala.util.Try

object Utils {

    // -----------------------------------------------------------------------------------------------------------------
    // Методы работы с числами
    // -----------------------------------------------------------------------------------------------------------------

    private val zeroEpsilon = 0.000001

    def roundWithScale(decimal: Double, scale: Int): Double =
        BigDecimal(decimal).setScale(scale, BigDecimal.RoundingMode.HALF_UP).toDouble

    def isNumberZero(number: Double): Boolean = math.abs(number) <= zeroEpsilon

    def number2number(number: Double) = if (isNumberZero(number)) 0.0 else number

    def convertExpressionToNumber(expression: String): Double = {
        val scale = 2
        val preconvertedExpression = expression
            .replaceAll("\\s", "")
            .replaceAll(",", ".")
            .replaceAll("\\(", "")
            .replaceAll("\\)", "")
            .replaceAll("\\{", "")
            .replaceAll("\\}", "")
        val divisionPattern = "^([\\-0-9]*)[\\/:]([\\-0-9]*)$".r
        val powPattern = "^([\\-0-9]*)\\^([\\-0-9]*)$".r
        val division = divisionPattern.findFirstMatchIn(preconvertedExpression).map { matchResult =>
            val leftNumber = matchResult.group(1).toDouble
            val rightNumber = matchResult.group(2).toDouble
            if (rightNumber == 0) throw new Exception("На ноль делить нельзя")
            roundWithScale(leftNumber / rightNumber, scale)
        }
        val pow = powPattern.findFirstMatchIn(preconvertedExpression).map { matchResult =>
            val downNumber = matchResult.group(1).toDouble
            val upNumber = matchResult.group(2).toDouble
            math.pow(downNumber, upNumber)
        }
        List(division, pow).flatten.headOption.getOrElse(roundWithScale(preconvertedExpression.toDouble, scale))
    }

    def getId: String = java.util.UUID.randomUUID.toString

    // -----------------------------------------------------------------------------------------------------------------
    // Методы работы с файлами
    // -----------------------------------------------------------------------------------------------------------------

    def deleteFileOrDir(paths: Seq[String]): Unit = {
        paths.foreach(deleteFileOrDir)
    }

    def deleteFileOrDir(path: String): Unit = {
        if (new File(path).exists)
            deleteFileOrDir(Paths.get(path))
    }

    def deleteFileOrDir(root: Path, deleteRoot: Boolean = true): Unit =
        Files.walkFileTree(root, new SimpleFileVisitor[Path] {
            override def visitFile(file: Path, attributes: BasicFileAttributes): FileVisitResult = {
                Files.delete(file)
                FileVisitResult.CONTINUE
            }

            override def postVisitDirectory(dir: Path, exception: IOException): FileVisitResult = {
                if (deleteRoot) Files.delete(dir)
                FileVisitResult.CONTINUE
            }
        })

    def readXls(xlsFilePath: String, sheetIndex: Int = 0): Array[Array[String]] = {
        val fileName = new File(xlsFilePath).getName
        val table = new ArrayBuffer[Array[String]]

        val workbook = """(\.xls|\.xlsx)$""".r.findFirstIn(fileName.toLowerCase).getOrElse("") match {
            case ".xls" =>
                new HSSFWorkbook(new FileInputStream(new File(xlsFilePath)))
            case _ =>
                new XSSFWorkbook(new FileInputStream(new File(xlsFilePath)))
        }
        val sheet = workbook.getSheetAt(sheetIndex)

        val rowIterator = sheet.rowIterator
        val cellsCount = {
            val rowIteratorForCount = sheet.rowIterator
            if (rowIteratorForCount.hasNext) rowIteratorForCount.next.getLastCellNum.toInt else 0
        }

        while (rowIterator.hasNext) {
            val row = rowIterator.next
            val cellsValues = for (cellIndex <- 0 until cellsCount) yield {
                val cell = row.getCell(cellIndex, Row.MissingCellPolicy.CREATE_NULL_AS_BLANK)
                val cellValue = new DataFormatter().formatCellValue(cell).replaceAll("\\s", "")
                if (cellValue.isEmpty) None else Some(cellValue)
            }
            if (cellsValues.flatten.nonEmpty) {
                table.append(cellsValues.map(cellValue =>cellValue.getOrElse("")).toArray)
            }
        }
        table.toArray
    }


    // -----------------------------------------------------------------------------------------------------------------
    // Методы работы с матрицами. Антагонистические игры
    // -----------------------------------------------------------------------------------------------------------------

        // -------------------------------------------------------------------------------------------------------------
        // * Симплекс
        // -------------------------------------------------------------------------------------------------------------

    // Поиск разрешающей строки по разрешающему столбцу
    private def getPermissiveRowIndex(matrix: Matrix, permissiveColumnIndex: Int): Option[Int] = Try {
        val matrixTransposed = matrix.transpose
        val freeColumnValues = matrixTransposed.matrix.head.init
        val permissiveColumnValues = matrixTransposed.matrix(permissiveColumnIndex).init
        val permissiveRowIndex = freeColumnValues.zip(permissiveColumnValues)
            .zipWithIndex
            .filter(x => !isNumberZero(x._1._2))
            .map {
                case ((freeColumnValue, permissiveColumnValue), rowIndex) =>
                    (freeColumnValue / permissiveColumnValue, rowIndex)
            }.filter(_._1 >= 0.0).minBy(_._1)._2
        Some(permissiveRowIndex)
    }.recover {
        case exception: Exception =>
            game_theory.logger.error(exception.getMessage)
            None
    }.get

    // Замена базиса
    private def changeBasicVariables[T: ClassTag](basicVariables: Array[T],
                                     variables: Array[T],
                                     permissiveRowIndex: Int,
                                     permissiveColumnIndex: Int): (Array[T], Array[T]) = {
        (
            basicVariables.patch(permissiveRowIndex, Array[T](variables(permissiveColumnIndex - 1)), 1),
            variables.patch(permissiveColumnIndex - 1, Array[T](basicVariables(permissiveRowIndex)), 1)
        )
    }

    // Жордановы исключения
    private def computeJordanExceptions(matrix: Matrix,
                                        permissiveRowIndex: Int,
                                        permissiveColumnIndex: Int): Matrix = {
        val matrixCopy = matrix
        Matrix(matrixCopy.matrix.zipWithIndex.map { case (row, rowIndex) =>
            row.zipWithIndex.map { case (value, columnIndex) =>
                if (rowIndex == permissiveRowIndex && columnIndex == permissiveColumnIndex) {
                    roundWithScale(1 / matrix.matrix(permissiveRowIndex)(permissiveColumnIndex), 5)
                } else if (rowIndex == permissiveRowIndex) {
                    roundWithScale(value / matrix.matrix(permissiveRowIndex)(permissiveColumnIndex), 5)
                } else if (columnIndex == permissiveColumnIndex) {
                    roundWithScale(-value / matrix.matrix(permissiveRowIndex)(permissiveColumnIndex), 5)
                } else {
                    roundWithScale(value - ((row(permissiveColumnIndex) * matrix.matrix(permissiveRowIndex)(columnIndex)) / matrix.matrix(permissiveRowIndex)(permissiveColumnIndex)), 5)
                }
            }
        })
    }

    // Поиск опорного решения
    @tailrec
    private def findReferenceSolution[T: ClassTag](matrix: Matrix,
                                      basicVariables: Array[T],
                                      variables: Array[T]): Option[(Matrix, Array[T], Array[T])] = {
        val matrixTransposed = matrix.transpose
        matrixTransposed.matrix.head.zipWithIndex.init.find(_._1 < 0.0) match {
            case Some((_, freeRowIndex)) =>
                matrix.matrix(freeRowIndex).zipWithIndex.tail.find(_._1 < 0.0) match {
                    case Some((_, permissiveColumnIndex)) =>
                        getPermissiveRowIndex(matrix, permissiveColumnIndex) match {
                            case Some(permissiveRowIndex) =>
                                val (newBasicVariables, newVariables) = changeBasicVariables(basicVariables, variables, permissiveRowIndex, permissiveColumnIndex)
                                val newMatrix = computeJordanExceptions(matrix, permissiveRowIndex, permissiveColumnIndex)
                                findReferenceSolution(newMatrix, newBasicVariables, newVariables)
                            case _ =>
                                None
                        }
                    case _ =>
                        None
                }
            case _ =>
                Some(matrix, basicVariables, variables)
        }
    }

    // Поиск оптимального решения
    @tailrec
    private def findOptimalSolution[T: ClassTag](matrix: Matrix,
                                    basicVariables: Array[T],
                                    variables: Array[T]): Option[(Matrix, Array[T], Array[T])] = {
        matrix.matrix.last.zipWithIndex.tail.find(_._1 > 0) match {
            case Some((_, permissiveColumnIndex)) =>
                getPermissiveRowIndex(matrix, permissiveColumnIndex) match {
                    case Some(permissiveRowIndex) =>
                        val (newBasicVariables, newVariables) = changeBasicVariables(basicVariables, variables, permissiveRowIndex, permissiveColumnIndex)
                        val newMatrix = computeJordanExceptions(matrix, permissiveRowIndex, permissiveColumnIndex)
                        findOptimalSolution(newMatrix, newBasicVariables, newVariables)
                    case _ =>
                        None
                }
            case _ =>
                Some(matrix, basicVariables, variables)
        }
    }

    def simplex[T: ClassTag](matrix: Matrix,
                basicVariables: Array[T],
                variables: Array[T]): Option[(Matrix, Array[T], Array[T])] = {
        findReferenceSolution(matrix, basicVariables, variables) flatMap {
            case (matrixReference, basicVariablesReference, variablesReference) =>
                findOptimalSolution(matrixReference, basicVariablesReference, variablesReference)
        }
    }

        // -------------------------------------------------------------------------------------------------------------
        // * Минимакс
        // -------------------------------------------------------------------------------------------------------------

    def minmaxSolution(matrix: Matrix): Option[(Array[Double], Array[Double])] = {
        val (max, maxIndex) = matrix.matrix.zipWithIndex.map { case (row, index) => (row.min, index) }.maxBy(_._1)
        val (min, minIndex) = matrix.transpose.matrix.zipWithIndex.map { case (column, index) => (column.max, index) }.minBy(_._1)
        if (max == min) {
            Some((
                Array.fill(matrix.getMatrixRowsCount)(0.0).patch(maxIndex, Array(1.0), 1),
                Array.fill(matrix.getMatrixColumnsCount)(0.0).patch(minIndex, Array(1.0), 1)
            ))
        } else {
            None
        }
    }

    def minmaxCost(matrix: Matrix): Option[(Int, Int, Double)] = {
        val (max, maxIndex) = matrix.matrix.zipWithIndex.map { case (row, index) => (row.min, index) }.maxBy(_._1)
        val (min, minIndex) = matrix.transpose.matrix.zipWithIndex.map { case (column, index) => (column.max, index) }.minBy(_._1)
        if (max == min) {
            Some((maxIndex, minIndex, matrix.matrix(maxIndex)(minIndex)))
        } else {
            None
        }
    }

        // -------------------------------------------------------------------------------------------------------------
        // * Аналитические методы
        // -------------------------------------------------------------------------------------------------------------

            // ---------------------------------------------------------------------------------------------------------
            // ** Метод обратных матриц
            // ---------------------------------------------------------------------------------------------------------

    def analyticalInverseMatrixMethod(matrixStrategy: Matrix): Either[String, Map[String, Any]] = Try {
        game_theory.logger.info("\n" + "*" * 10 + " INVERSE MATRIX METHOD " + "*" * 10 + "\n")
        val unitMatrix = Matrix(Array(Array.fill(matrixStrategy.getMatrixRowsCount)(1.0)))
        game_theory.logger.info("\nunitMatrix:\n" + formatTable(unitMatrix.matrix.map(_.toSeq).toSeq) + "\n")
        val unitMatrixTransposed = unitMatrix.transpose
        game_theory.logger.info("\nunitMatrixTransposed:\n" + formatTable(unitMatrixTransposed.matrix.map(_.toSeq).toSeq) + "\n")
        val matrixStrategyInverse = matrixStrategy.inverse
        game_theory.logger.info("\nmatrixStrategy:\n" + formatTable(matrixStrategy.matrix.map(_.toSeq).toSeq) + "\n")
        game_theory.logger.info("\nmatrixStrategyInverse:\n" + formatTable(matrixStrategyInverse.matrix.map(_.toSeq).toSeq) + "\n")
        val gameCost = (unitMatrix * matrixStrategyInverse * unitMatrixTransposed).inverse.matrix.head.head
        game_theory.logger.info("\ngameCost= 1 / (unitMatrix * matrixStrategyInverse * unitMatrixTransposed): " + gameCost + "\n")
        val firstGamerStrategyNumerator = unitMatrix * matrixStrategyInverse
        val firstGamerStrategy = firstGamerStrategyNumerator.copy(firstGamerStrategyNumerator.matrix.map(_.map(_ * gameCost)))
        game_theory.logger.info("\nfirstGamerStrategy = (unitMatrix * matrixStrategyInverse) / (unitMatrix * matrixStrategyInverse * unitMatrixTransposed):\n" + formatTable(firstGamerStrategy.matrix.map(_.toSeq).toSeq) + "\n")
        val secondGamerStrategyNumerator = matrixStrategyInverse * unitMatrixTransposed
        val secondGamerStrategy = secondGamerStrategyNumerator.copy(secondGamerStrategyNumerator.matrix.map(_.map(_ * gameCost)))
        game_theory.logger.info("\nsecondGamerStrategy = (matrixStrategyInverse * unitMatrixTransposed) / (unitMatrix * matrixStrategyInverse * unitMatrixTransposed):\n" + formatTable(secondGamerStrategy.matrix.map(_.toSeq).toSeq) + "\n")

        Right(Map(
            "firstGamerStrategy" -> firstGamerStrategy.matrix.flatten.map(matrixElement => roundWithScale(matrixElement, scale = 2)),
            "secondGamerStrategy" -> secondGamerStrategy.matrix.flatten.map(matrixElement => roundWithScale(matrixElement, scale = 2)),
            "gameCost" -> roundWithScale(gameCost, scale = 2)
        ))
    }.recover {
        case exception: Exception =>
            val errorMessage = exception.getMessage
            game_theory.logger.error("Inverse Matrix Method error: " + errorMessage)
            Left(errorMessage)
    }.get

            // ---------------------------------------------------------------------------------------------------------
            // ** Аналитический метод решения выпукло-вогнутых игр
            // ---------------------------------------------------------------------------------------------------------

    def analyticalConvexConcaveGamesMethod(matrixStrategyKernel: GameCorePolynomial): Either[String, Map[String, Double]] = {
        game_theory.logger.info("\n" + "*" * 10 + " ANALYTICAL CONVEX CONCAVE GAMES METHOD " + "*" * 10 + "\n")
        val firstDerivativeX = matrixStrategyKernel.derivative(0)
        val firstDerivativeY = matrixStrategyKernel.derivative(1)
        val secondDerivativeX = firstDerivativeX.derivative(0).polynomial.head._1
        val secondDerivativeY = firstDerivativeY.derivative(1).polynomial.head._1
        if (secondDerivativeX < 0.0 && secondDerivativeY > 0.0) {
            val Array(solutionX, solutionY) = GameCorePolynomial.solve(Array(firstDerivativeX, firstDerivativeY))
                .map(solution => roundWithScale(solution, scale = 3))
            if (solutionX < 0.0 || solutionY < 0.0) {
                Left(s"x = $solutionX, y = $solutionY; Игра не имеет оптимальных стратегий")
            } else {
                val saddlePoint = roundWithScale(matrixStrategyKernel.compute(Array(solutionX, solutionY)), scale = 3)
                Right(Map(
                    "solutionX" -> solutionX,
                    "solutionY" -> solutionY,
                    "saddlePoint" -> saddlePoint
                ))
            }
        } else {
            Left("Игра не является выпукло-вогнутой")
        }
    }

        // -------------------------------------------------------------------------------------------------------------
        // * Численные методы
        // -------------------------------------------------------------------------------------------------------------

            // ---------------------------------------------------------------------------------------------------------
            // ** Браун - Робинсон
            // ---------------------------------------------------------------------------------------------------------

    private def findOptimalStrategies(firstGamerWins: Array[Double], secondGamerLosses: Array[Double]): (Int, Int) = {
        if (firstGamerWins.isEmpty || secondGamerLosses.isEmpty) {
            (0, 0)
        } else {
            val firstMaxWinAndStrategy = firstGamerWins.zipWithIndex.maxBy(_._1)
            val firstOptimalStrategy = firstMaxWinAndStrategy._2
            val secondMinLossAndStrategy = secondGamerLosses.zipWithIndex.minBy(_._1)
            val secondOptimalStrategy = secondMinLossAndStrategy._2
            val firstGamerMaxWins = firstGamerWins.zipWithIndex.filter(_._1 == firstMaxWinAndStrategy._1)
            val secondGamerMaxLosses = secondGamerLosses.zipWithIndex.filter(_._1 == secondMinLossAndStrategy._1)
            (
                if (firstGamerMaxWins.length > 1) {
                    firstGamerMaxWins(scala.util.Random.nextInt(firstGamerMaxWins.length))._2
                } else {
                    firstOptimalStrategy
                },
                if (secondGamerMaxLosses.length > 1) {
                    secondGamerMaxLosses(scala.util.Random.nextInt(secondGamerMaxLosses.length))._2
                } else {
                    secondOptimalStrategy
                }
            )
        }
    }

    private def getGameCostUp(firstGamerWins: Array[Double], iterationNumber: Int): Double = {
        firstGamerWins.max / iterationNumber
    }

    private def getGameCostLoss(secondGamerLosses: Array[Double], iterationNumber: Int): Double = {
        secondGamerLosses.min / iterationNumber
    }

    def brownRobinson(matrixStrategy: Matrix): Either[String, Map[String, Any]] = Try {
        game_theory.logger.info("\n" + "*"*10 + " BROWN-ROBINSON METHOD " + "*"*10 + "\n")
        var brownRobinsonMatrixAlgorithm = List.empty[MSA]

        var iterationNumber = 0

        var firstGamerStrategiesChoices = Array.fill(matrixStrategy.getMatrixRowsCount)(0)
        var secondGamerStrategiesChoices = Array.fill(matrixStrategy.getMatrixColumnsCount)(0)

        var firstGamerWins = Array.empty[Double]
        var secondGamerLosses = Array.empty[Double]

        var gameCostsAveragesUp = List.empty[Double]
        var gamesCostsAveragesLosses = List.empty[Double]

        var epsilon: Option[Double] = None

        while (epsilon.isEmpty || epsilon.get > 0.1) {
            iterationNumber += 1

            val (firstGamerOptimalStrategy, secondGamerOptimalStrategy) = findOptimalStrategies(firstGamerWins, secondGamerLosses)

            firstGamerStrategiesChoices = firstGamerStrategiesChoices.updated(
                firstGamerOptimalStrategy,
                firstGamerStrategiesChoices(firstGamerOptimalStrategy) + 1
            )
            secondGamerStrategiesChoices = secondGamerStrategiesChoices.updated(
                secondGamerOptimalStrategy,
                secondGamerStrategiesChoices(secondGamerOptimalStrategy) + 1
            )

            firstGamerWins = firstGamerWins
                .zipAll(matrixStrategy.matrix.transpose.apply(secondGamerOptimalStrategy), 0.0, 0.0)
                .map { case (oldWin, newWin) => oldWin + newWin }
            secondGamerLosses = secondGamerLosses
                .zipAll(matrixStrategy.matrix(firstGamerOptimalStrategy), 0.0, 0.0)
                .map { case (oldLoss, newLoss) => oldLoss + newLoss }


            val currentGameCostUp = getGameCostUp(firstGamerWins, iterationNumber)
            gameCostsAveragesUp = gameCostsAveragesUp :+ currentGameCostUp
            val currentGameCostLoss = getGameCostLoss(secondGamerLosses, iterationNumber)
            gamesCostsAveragesLosses = gamesCostsAveragesLosses :+ currentGameCostLoss

            epsilon = Some(gameCostsAveragesUp.min - gamesCostsAveragesLosses.max)

            brownRobinsonMatrixAlgorithm = brownRobinsonMatrixAlgorithm :+ Map(
                "iterationNumber" -> iterationNumber,
                "firstOptimalStrategy" -> (firstGamerOptimalStrategy + 1),
                "secondOptimalStrategy" -> (secondGamerOptimalStrategy + 1),
                "firstGamerWins" -> firstGamerWins,
                "secondGamerLosses" -> secondGamerLosses,
                "gameCostAverageUp" -> roundWithScale(currentGameCostUp, scale = 2),
                "gameCostAverageLoss" -> roundWithScale(currentGameCostLoss, scale = 2),
                "epsilon" -> epsilon.map(epsilonValue => roundWithScale(epsilonValue, scale = 2))
            )
        }
        val gameCostMaxLoss = roundWithScale(gamesCostsAveragesLosses.max, scale = 2)
        val gameCostMinWin = roundWithScale(gameCostsAveragesUp.min, scale = 2)
        game_theory.logger.info("\nfirstGamerStrategy: (" + firstGamerStrategiesChoices.map(choice => choice.toDouble / iterationNumber).mkString(",") + ")\n")
        game_theory.logger.info("\nsecondGamerStrategy: (" + secondGamerStrategiesChoices.map(choice => choice.toDouble / iterationNumber).mkString(",") + ")\n")
        game_theory.logger.info("\ngameCost: " + (gameCostMaxLoss + gameCostMinWin) / 2 + "\n")
        game_theory.logger.info("\nepsilon: " + epsilon.map(epsilonValue => epsilonValue).getOrElse("error") + ")\n")
        Right(Map(
            "brownRobinsonMatrixAlgorithm" -> brownRobinsonMatrixAlgorithm,
            "firstGamerStrategy" -> firstGamerStrategiesChoices.map(choice => roundWithScale(choice.toDouble / iterationNumber, scale = 2)),
            "secondGamerStrategy" -> secondGamerStrategiesChoices.map(choice => roundWithScale(choice.toDouble / iterationNumber, scale = 2)),
            "gameCost" -> List(gameCostMaxLoss, gameCostMinWin),
            "gameCostAverage" -> roundWithScale((gameCostMaxLoss + gameCostMinWin) / 2, scale = 2),
            "epsilon" -> epsilon.map(epsilonValue => roundWithScale(epsilonValue, scale = 2))
        ))
    }.recover {
        case exception: Exception =>
            val errorMessage = exception.getMessage
            game_theory.logger.error("Brown-Robinson Method error: " + errorMessage)
            Left(errorMessage)
    }.get

            // ---------------------------------------------------------------------------------------------------------
            // ** Численный метод решения выпукло-вогнутых игр
            // ---------------------------------------------------------------------------------------------------------

    private val differenceEpsilon = 0.03
    private val lastIterationsCountLimit = 5

    @tailrec
    def numericalConvexConcaveGamesMethod(matrixStrategyKernel: GameCorePolynomial,
                                          partitionParameter: Int = 1,
                                          lastIterationsCountWithMinDifference: Int = 0,
                                          previousGameCostOp: Option[Double] = None,
                                          solutions: Array[MSA] = Array()): Array[MSA] = {
        if (lastIterationsCountWithMinDifference == lastIterationsCountLimit) {
            solutions
        } else {
            val dimensions = partitionParameter + 1
            val approximation = Matrix.fill(dimensions, dimensions, 0.0).matrix.zipWithIndex.map { case (row, rowIndex) =>
                row.zipWithIndex.map { case (_, columnIndex) =>
                    roundWithScale(
                        matrixStrategyKernel.compute(Array(rowIndex.toDouble / partitionParameter, columnIndex.toDouble / partitionParameter)),
                        scale = 3
                    )
                }
            }
            game_theory.logger.info(s"\nN=$partitionParameter\n${formatTable(approximation.map(_.toSeq).toSeq)}")
            val (solutionX, solutionY, gameCost, comment) = minmaxCost(Matrix(approximation)).map {
                case (rowIndex, columnIndex, cost) =>
                    (
                        rowIndex.toDouble / partitionParameter,
                        columnIndex.toDouble / partitionParameter,
                        cost,
                        "Есть седловая точка"
                    )
            }.getOrElse {
                brownRobinson(Matrix(approximation)).map { brownRobinsonResult =>
                    val brownRobinsonMatrixAlgorithm = brownRobinsonResult("brownRobinsonMatrixAlgorithm").asInstanceOf[List[MSA]]
                    val rowIndex = brownRobinsonMatrixAlgorithm.last("firstOptimalStrategy").toString.toDouble - 1.0
                    val columnIndex = brownRobinsonMatrixAlgorithm.last("secondOptimalStrategy").toString.toDouble - 1.0
                    (
                        rowIndex / partitionParameter,
                        columnIndex / partitionParameter,
                        brownRobinsonResult("gameCostAverage").toString.toDouble,
                        "Седловой точки нет, решение методом Брауна - Робинсон"
                    )
                }.getOrElse(throw new Exception("Нет решений"))
            }
            game_theory.logger.info(s"\n$comment\nx=$solutionX; y=$solutionY; H=$gameCost")
            val isMinDifference = previousGameCostOp.map { previousGameCost =>
                math.abs(previousGameCost - gameCost) <= differenceEpsilon
            }
            numericalConvexConcaveGamesMethod(
                matrixStrategyKernel,
                partitionParameter + 1,
                if (isMinDifference.contains(true)) lastIterationsCountWithMinDifference + 1 else 0,
                Some(gameCost),
                solutions :+ Map(
                    "partitionParameter" -> partitionParameter,
                    "approximation" -> approximation,
                    "solutionX" -> roundWithScale(solutionX, scale = 3),
                    "solutionY" -> roundWithScale(solutionY, scale = 3),
                    "gameCost" -> roundWithScale(gameCost, scale = 3),
                    "comment" -> comment
                )
            )
        }
    }

    // -----------------------------------------------------------------------------------------------------------------
    // Методы работы с матрицами. Неантагонистические игры
    // -----------------------------------------------------------------------------------------------------------------

        // -------------------------------------------------------------------------------------------------------------
        // * Равновесие по Нэшу
        // -------------------------------------------------------------------------------------------------------------

    private def findNashEquilibriumInMixedStrategies(matricesStrategy: Array[Matrix]): Either[String, MSA] = {
        var exceptions = matricesStrategy.zipWithIndex.foldLeft(Array.empty[String]) { case (errors, (matrix, matrixIndex)) =>
            if (!matrix.IsNonDegenerateMatrix) errors :+ s"Матрица $matrixIndex вырожденная. Нет смешанной ситуации равновесия"
            else errors
        }
        if (exceptions.nonEmpty) {
            game_theory.logger.error("Find Nash Equilibrium In Clear Strategies error: " + exceptions.mkString("\n"))
            Left(exceptions.mkString("\n"))
        } else {
            val nashEquilibriumInMixedStrategies = matricesStrategy.map(analyticalInverseMatrixMethod)
                .flatMap {
                    case Left(errMsg) =>
                        exceptions = exceptions :+ errMsg
                        None
                    case Right(analyticalInverseMatrixMethodResult) =>
                        Some(analyticalInverseMatrixMethodResult)
                }
            val gameCosts = nashEquilibriumInMixedStrategies.map(_("gameCost"))
            val strategies = nashEquilibriumInMixedStrategies.map(_("firstGamerStrategy")).reverse
            val result =
                if (strategies.map(_.asInstanceOf[Array[Double]]).exists(_.exists(_ < 0.0))) {
                    Map.empty[String, Any]
                } else {
                    val nashEquilibriumInMixed = strategies.zip(gameCosts).map { case (gamerStrategy, gameCost) =>
                        Map("gamerStrategy" -> gamerStrategy, "gameCost" -> gameCost)
                    }
                    Map("nashEquilibriumInMixedStrategies" -> nashEquilibriumInMixed)
                }
            if (exceptions.nonEmpty) {
                game_theory.logger.error("Find Nash Equilibrium In Clear Strategies error: " + exceptions.mkString("\n"))
                Left(exceptions.mkString("\n"))
            } else {
                Right(result)
            }
        }
    }

     private def findNashEquilibriumInClearStrategies(matricesStrategyAndIsTransposed: Array[(Matrix, Boolean)]): Either[String, MSA] =
         Try {
             val nashEquilibriumInClear = matricesStrategyAndIsTransposed
                 .flatMap { case (matrixStrategy, isTransposed) =>
                     matrixStrategy.matrix.zipWithIndex.flatMap { case (row, rowIndex) =>
                         row.zipWithIndex.foldLeft(Array.empty[(Double, Int, Int)]) { case (maxElements, (element, columnIndex)) =>
                             val (sourceRowIndex, sourceColumnIndex) = if (!isTransposed) (rowIndex, columnIndex) else (columnIndex, rowIndex)
                             maxElements.headOption match {
                                 case Some(maxElem) =>
                                     if (element > maxElem._1) Array((element, sourceRowIndex, sourceColumnIndex))
                                     else if (element == maxElem._1) maxElements :+ (element, sourceRowIndex, sourceColumnIndex)
                                     else maxElements
                                 case _ =>
                                     Array((element, sourceRowIndex, sourceColumnIndex))
                             }
                         }
                     }
                 }
                 .groupBy { case (_, rowIndex, columnIndex) => (rowIndex, columnIndex) }
                 .filter(_._2.length > 1).values.flatten
                 .map { elementIndices =>
                     Map("value" -> elementIndices._1, "rowIndex" -> elementIndices._2, "columnIndex" -> elementIndices._3).asInstanceOf[MSA]
                 }.toArray
             Right(Map("nashEquilibriumInClearStrategies" -> nashEquilibriumInClear))
         }.recover {
             case exception: Exception =>
                 val errorMessage = exception.getMessage
                 game_theory.logger.error("Find Nash Equilibrium In Clear Strategies error: " + errorMessage)
                 Left(errorMessage)
         }.get

    private def hasStrictlyDominantStrategy(matricesStrategy: Matrix): Boolean = {
        matricesStrategy.matrix.map(row => row.zipWithIndex.maxBy(_._1)).map(_._2).toSet.size == 1
    }

    def findNashEquilibrium(matricesStrategyAndIsTransposed: Array[(Matrix, Boolean)]): Either[String, MSA] = {
        if (matricesStrategyAndIsTransposed.head._1.getMatrixRowsCount == 2 &&
            matricesStrategyAndIsTransposed.head._1.getMatrixColumnsCount == 2) {
            if (matricesStrategyAndIsTransposed.exists { case (matrixStrategy, _) => hasStrictlyDominantStrategy(matrixStrategy) }) {
                findNashEquilibriumInClearStrategies(matricesStrategyAndIsTransposed).flatMap { nashEquilibriumInClearStrategies =>
                    findNashEquilibriumInMixedStrategies(matricesStrategyAndIsTransposed.map(_._1)).map { nashEquilibriumInMixedStrategies =>
                        nashEquilibriumInClearStrategies ++ nashEquilibriumInMixedStrategies
                    }
                }
            } else {
                findNashEquilibriumInClearStrategies(matricesStrategyAndIsTransposed).flatMap { nashEquilibriumInClearStrategies =>
                    val nashEquilibriumInClearStrategiesResult = nashEquilibriumInClearStrategies("nashEquilibriumInClearStrategies").asInstanceOf[Array[MSA]]
                    if (nashEquilibriumInClearStrategiesResult.nonEmpty) {
                        if (nashEquilibriumInClearStrategiesResult.length >= 4) {
                            findNashEquilibriumInMixedStrategies(matricesStrategyAndIsTransposed.map(_._1)).map { nashEquilibriumInMixedStrategies =>
                                nashEquilibriumInClearStrategies ++ nashEquilibriumInMixedStrategies
                            }
                        } else {
                            Right(nashEquilibriumInClearStrategies)
                        }
                    } else {
                        findNashEquilibriumInMixedStrategies(matricesStrategyAndIsTransposed.map(_._1))
                    }
                }
            }
        } else {
            findNashEquilibriumInClearStrategies(matricesStrategyAndIsTransposed)
        }
    }

        // -------------------------------------------------------------------------------------------------------------
        // * Оптимальность по Парето
        // -------------------------------------------------------------------------------------------------------------

    def findOptimalPareto(matricesStrategy: Array[Matrix]): Either[String, MSA] = Try {
        val matrixWithAllWins = matricesStrategy.head.matrix.zipWithIndex.map { case (row, rowIndex) =>
            row.zipWithIndex.map { case (element, columnIndex) =>
                Array(element) ++ matricesStrategy.tail.map { otherMatrixStrategy =>
                    otherMatrixStrategy.matrix(rowIndex)(columnIndex)
                }
            }
        }
        val optimalElements = matrixWithAllWins.zipWithIndex.flatMap { case (row, rowIndex) =>
            row.zipWithIndex.flatMap { case (element, columnIndex) =>
                val isOptimal = !matrixWithAllWins.zipWithIndex.exists { case (otherColumns, otherRowIndex) =>
                    otherColumns.zipWithIndex.exists { case (otherElement, otherColumnIndex) =>
                        if (rowIndex == otherRowIndex && columnIndex == otherColumnIndex) {
                            false
                        } else {
                            val elementsZipped = otherElement.zip(element)
                            if (elementsZipped.head._1 == elementsZipped.head._2) {
                                !elementsZipped.tail.exists { case (other, current) => other <= current }
                            } else if (elementsZipped.head._1 > elementsZipped.head._2) {
                                !elementsZipped.tail.exists { case (other, current) => other < current }
                            } else {
                                false
                            }
                        }
                    }
                }
                if (isOptimal) Some(Map("value" -> element, "rowIndex" -> rowIndex, "columnIndex" -> columnIndex))
                else None
            }
        }
        Right(Map("optimalPareto" -> optimalElements))
    }.recover {
        case exception: Exception =>
            val errorMessage = exception.getMessage
            game_theory.logger.error("Find Optimal Pareto error: " + errorMessage)
            Left(errorMessage)
    }.get


    // -------------------------------------------------------------------------------------------------------------
    // * Игры поиска
    // -------------------------------------------------------------------------------------------------------------

    private def planeIntersectionParameterHandler(denominator: Double, radius: Double): Double = {
        val parameter = radius / denominator
        if (parameter < 0.0) -parameter else parameter
    }

    private def planeIntersectionParameter(radius: Double, point: Point): Option[Double] = {
        (point.x - radius, point.y - radius, point.z - radius) match {
            case (0.0, 0.0, 0.0) => None
            case (denominatorX, 0.0, 0.0) => Some(planeIntersectionParameterHandler(denominatorX, radius))
            case (0.0, denominatorY, 0.0) => Some(planeIntersectionParameterHandler(denominatorY, radius))
            case (0.0, 0.0, denominatorZ) => Some(planeIntersectionParameterHandler(denominatorZ, radius))
            case (0.0, denominatorY, denominatorZ) =>
                Some(List(
                    planeIntersectionParameterHandler(denominatorY, radius),
                    planeIntersectionParameterHandler(denominatorZ, radius)
                ).min)
            case (denominatorX, 0.0, denominatorZ) =>
                Some(List(
                    planeIntersectionParameterHandler(denominatorX, radius),
                    planeIntersectionParameterHandler(denominatorZ, radius)
                ).min)
            case (denominatorX, denominatorY, 0.0) =>
                Some(List(
                    planeIntersectionParameterHandler(denominatorX, radius),
                    planeIntersectionParameterHandler(denominatorY, radius)
                ).min)
            case (denominatorX, denominatorY, denominatorZ) =>
                Some(List(
                    planeIntersectionParameterHandler(denominatorX, radius),
                    planeIntersectionParameterHandler(denominatorY, radius),
                    planeIntersectionParameterHandler(denominatorZ, radius)
                ).min)
        }
    }


    private def generateFirstGamerStrategyOnCube(pointsCount: Int, cubeDimensions: Double, epsilon: Double): List[Point] = {
        val radius = cubeDimensions / 2.0
        val pointsIndices = (0 until pointsCount).toList
        val points = pointsIndices.map { pointIndex =>
            val theta = (2.0 * math.Pi * pointIndex) / ((1 + math.pow(5.0, 0.5)) / 2.0)
            val phi = math.acos(1.0 - (2.0 * pointIndex / pointsCount))

            val x = radius * Math.cos(theta) * Math.sin(phi) + radius
            val y = radius * Math.sin(theta) * Math.sin(phi) + radius
            val z = radius * Math.cos(phi) + radius

            Point(Utils.roundWithScale(x, 10), Utils.roundWithScale(y, 10), Utils.roundWithScale(z, 10))
        }
        points.filter(point => point.notIntersectWith(points.filter(_ != point), epsilon))

    }

    private def generateSecondGamerStrategyOnCube(pointsCount: Int, cubeDimensions: Double): List[Point] = {
        val radius = cubeDimensions / 2.0
        val pointsIndices = (0 until pointsCount).toList
        val points = pointsIndices.map { pointIndex =>
            val theta = (2.0 * math.Pi * pointIndex) / ((1 + math.pow(5.0, 0.5)) / 2.0)
            val phi = math.acos(1.0 - (2.0 * pointIndex / pointsCount))

            val x = radius * Math.cos(theta) * Math.sin(phi) + radius
            val y = radius * Math.sin(theta) * Math.sin(phi) + radius
            val z = radius * Math.cos(phi) + radius

            Point(Utils.roundWithScale(x, 10), Utils.roundWithScale(y, 10), Utils.roundWithScale(z, 10))
        }
        points
    }

    def searchGame(pointsCount: Int, cubeDimensions: Double, epsilon: Double) = {
        val gamesCount = 120
        val firstGamerPoints = generateFirstGamerStrategyOnCube(pointsCount, cubeDimensions, epsilon)
        val secondGamerPointsWithWins = generateSecondGamerStrategyOnCube(gamesCount, cubeDimensions) map {
            case secondPoint if secondPoint.notIntersectWith(firstGamerPoints, epsilon) => (secondPoint, true)
            case secondPoint => (secondPoint, false)
        }
        val gameCost = Utils.roundWithScale(
            secondGamerPointsWithWins.count(_._2 == false).toDouble / secondGamerPointsWithWins.length,
            3
        )
        Map(
            "cubeDimensions" -> cubeDimensions,
            "firstGamerPoints" -> firstGamerPoints.map(_.toMap),
            "secondGamerPoints" -> secondGamerPointsWithWins.map(_._1.toMap),
            "gameCost" -> gameCost,
            "gamesCount" -> secondGamerPointsWithWins.length,
            "epsilon" -> epsilon
        )
    }


    // -------------------------------------------------------------------------------------------------------------
        // * Позиционные игры
        // -------------------------------------------------------------------------------------------------------------

    private var tree = Map.empty[String, TreeNode]

    def setTree(newTree: Map[String, TreeNode]) = { tree = newTree }
    def getTree = tree

    private def generateTreeHandler(
                                       root: TreeNode,
                                       lastChildInBound: Boolean,
                                       treeDepth: Int,
                                       gamersCount: Int,
                                       gamersStrategiesCount: Map[Int, Int],
                                       winMin: Int,
                                       winMax: Int,
                                       treeLevel: Int = 0
                                   ): Unit = {
        if (treeLevel != treeDepth) {
            val gamerIndex = treeLevel % gamersCount
            val strategiesCount = gamersStrategiesCount(gamerIndex)

            val leaf = treeLevel == (treeDepth - 2)
            val additionalLeavesIndices = if (treeLevel > 0) {
                val availableCountOfLeaves = if (lastChildInBound) strategiesCount - 1 else strategiesCount
                (0 until availableCountOfLeaves).map(_ => scala.util.Random.nextInt(strategiesCount + 1)).toList
            } else {
                List()
            }
            val children = if (root.leaf) {
                List()
            } else {
                (0 until strategiesCount).map { strategyIndex =>
                    val wins = if (leaf || additionalLeavesIndices.contains(strategyIndex)) {
                        (0 until gamersCount).map(_ => winMax - scala.util.Random.nextInt(winMax - winMin)).toList
                    } else {
                        List()
                    }
                    TreeNode(
                        Utils.getId,
                        List(),
                        Some(root.id),
                        if (wins.nonEmpty) List(wins) else List(),
                        treeLevel + 1,
                        if (additionalLeavesIndices.contains(strategyIndex)) true else leaf
                    )
                }.toList
            }

            val updatedRoot = root.copy(children = children.map(_.id))
            tree = tree + (updatedRoot.id -> updatedRoot)
            children.zipWithIndex.foreach { case (child, childIndex) =>
                generateTreeHandler(
                    child, childIndex == children.length - 1,
                    treeDepth, gamersCount, gamersStrategiesCount,
                    winMin, winMax, treeLevel + 1
                )
            }
        }
    }

    def generateTree(treeDepth: Int, gamersCount: Int, strategiesCount: List[Int], winMin: Int, winMax: Int): Unit = {
        tree = Map.empty[String, TreeNode]
        generateTreeHandler(
            TreeNode(Utils.getId, List(), None, List(), 0, leaf = false, root = true),
            true,
            treeDepth,
            gamersCount,
            strategiesCount.zipWithIndex.map { case (strategy, gamerIndex) => gamerIndex -> strategy }.toMap,
            winMin,
            winMax
        )
    }

    private def unBestChildren(rootId: String): Unit = {
        val root = tree.find(_._1 == rootId).getOrElse(throw new Exception(s"node with id=${rootId} not found"))._2
        val unBestedChildren = root.children.map { childId =>
            val child = tree.find(_._1 == childId).getOrElse(throw new Exception(s"node with id=${childId} not found"))._2
            child.copy(best = false)
        }
        tree = tree ++ unBestedChildren.map(child => child.id -> child).toMap
        unBestedChildren.foreach(child => unBestChildren(child.id))
    }

    def computePositionGame(root: TreeNode, gamersCount: Int): TreeNode = {
        val childrenWithWins = root.children.map { childId =>
            val child = tree.getOrElse(childId, throw new Exception(s"node with id=${childId} not found"))
            if (child.wins.isEmpty) {
                computePositionGame(child, gamersCount)
            } else {
                child
            }
        }
        val childrenWithMaxWins = childrenWithWins.groupBy { child =>
            val parentId = root.id
            val parent = tree.getOrElse(parentId, throw new Exception(s"node with id=${parentId} not found"))
            val gamerIndex = parent.level % gamersCount
            child.wins.map(win => win(gamerIndex)).max
        }.maxBy(_._1)._2
        childrenWithWins.filter(child => !childrenWithMaxWins.contains(child)).foreach { child =>
            tree = tree + (child.id -> child.copy(best = false))
            unBestChildren(child.id)
        }
        val updatedRoot = root.copy(wins = childrenWithMaxWins.flatMap(_.wins), best = true)
        tree = tree + (updatedRoot.id -> updatedRoot)
        tree = tree ++ childrenWithMaxWins.map( child => child.id -> child.copy(best = true)).toMap
        updatedRoot
    }

    @tailrec
    def setColorForTree(
                           nodes: List[(TreeNode, String)],
                           availableColors: List[String],
                           first: Boolean = true
                       ): Unit = {
        if (nodes.nonEmpty) {
            val parentsWithColor = nodes.zipWithIndex.flatMap { case ((node, color), colorIndex) =>
                val newColor = if (first) availableColors(colorIndex) else color
                val updatedNode = node.copy(color = newColor)
                tree = tree + (updatedNode.id -> updatedNode)
                tree.find(_._1 == node.parent.getOrElse("")).map(node => (node._2, newColor))
            }
            setColorForTree(parentsWithColor, availableColors, false)
        }
    }

    def setGamer(gamersCount: Int, availableGamers: Map[Int, String]): Unit = {
        tree = tree.map { case (id, node) =>
            id -> node.copy(gamer = if (node.leaf) "" else availableGamers(node.level % gamersCount))
        }
    }

    // -------------------------------------------------------------------------------------------------------------
    // * Кооперативные игры
    // -------------------------------------------------------------------------------------------------------------

    private def factorial(number: Int): Int =
        (2 to number).foldLeft(1) { case (gamerNumber, numberFactorial) => numberFactorial * gamerNumber }

    def computeShapleyVector(gamersCount: Int, characteristicFunction: Map[Coalition, Double]): Map[Int, Double] = {
        val factor = 1.0 / factorial(gamersCount)
        (1 to gamersCount).foldLeft(Map.empty[Int, Double]) { case (shapleyVector, gamerId) =>
            val characteristicFunctionForCoalitionsWithGamerId = characteristicFunction.filter(_._1.contains(gamerId))
            val shapleyComponent = factor * characteristicFunctionForCoalitionsWithGamerId.map { characteristicFuncForCoalsWithGamerId =>
                val coalitionSize = characteristicFuncForCoalsWithGamerId._1.length
                val coalitionBenefit = characteristicFuncForCoalsWithGamerId._2
                val coalitionBenefitWithoutGamerId = characteristicFunction(characteristicFuncForCoalsWithGamerId._1.filter(_ != gamerId))
                factorial(coalitionSize - 1) * factorial(gamersCount - coalitionSize) * (coalitionBenefit - coalitionBenefitWithoutGamerId)
            }.sum
            shapleyVector + (gamerId -> shapleyComponent)
        }
    }

    def isIndividualRationalizationConditionMet(
                                                 shapleyVector: Map[Int, Double],
                                                 characteristicFunction: Map[Coalition, Double]
                                               ): (Boolean, List[Int]) = {
        shapleyVector.foldLeft((true, List.empty[Int])) {
            case ((individualRationalizationConditionIsTrue, badGamerIds), (gamerId, shapleyComponent)) =>
                val newIndividualRationalizationConditionIsTrue = shapleyComponent >= characteristicFunction(List(gamerId))
                (
                  individualRationalizationConditionIsTrue && newIndividualRationalizationConditionIsTrue,
                  if (newIndividualRationalizationConditionIsTrue) badGamerIds else badGamerIds :+ gamerId
                )
        }
    }

    private val groupRationalizationZeroEpsilon = 0.1

    def isGroupRationalizationConditionMet(
                                            gamersCount: Int,
                                            shapleyVector: Map[Int, Double],
                                            characteristicFunction: Map[Coalition, Double]
                                          ): Boolean = {
        val shapleyComponentsSum = shapleyVector.values.sum
        val coalitionsAllBenefit = characteristicFunction.find(_._1.toSet == (1 to gamersCount).toSet).map(_._2).getOrElse(0.0)
        shapleyComponentsSum - coalitionsAllBenefit <= groupRationalizationZeroEpsilon
    }

    private def checkCooperativeGameOn(
                                        characteristicFunction: Map[Coalition, Double],
                                        coalitionsPairsFilter: List[Coalition] => Boolean,
                                        coalitionsPairsCheckFunction: List[Coalition] => (Boolean, List[Coalition], List[Coalition])
                                      ): (Boolean, List[List[Coalition]], List[List[Coalition]]) = {
        val coalitions = characteristicFunction.keys.toList.filter(_.nonEmpty)
        val coalitionsPairs = coalitions.combinations(2).filter(coalitionsPairsFilter).toList
        coalitionsPairs.foldLeft((true, List.empty[List[Coalition]], List.empty[List[Coalition]])) {
            case ((checkResult, goodCoalitionsPairs, badCoalitionsPairs), coalitionPair) =>
                val (newCheckResult, newGoodCoalitionsPairs, newBadCoalitionsPairs) = coalitionsPairsCheckFunction(coalitionPair)
                (
                  checkResult && newCheckResult,
                  if (newGoodCoalitionsPairs.nonEmpty) goodCoalitionsPairs :+ newGoodCoalitionsPairs else goodCoalitionsPairs,
                  if (newBadCoalitionsPairs.nonEmpty) badCoalitionsPairs :+ newBadCoalitionsPairs else badCoalitionsPairs
                )
        }
    }

    def isSuperadditive(characteristicFunction: Map[Coalition, Double]): (Boolean, List[List[Coalition]], List[List[Coalition]]) =
        checkCooperativeGameOn(
            characteristicFunction,
            coalitionsPairsFilter = (coalitionPair: List[Coalition]) => coalitionPair(0).intersect(coalitionPair(1)).isEmpty,
            coalitionsPairsCheckFunction = (coalitionPair: List[Coalition]) => {
                val List(coalitionFirst, coalitionSecond) = coalitionPair
                val superadditive = characteristicFunction
                  .find(_._1.toSet == (coalitionFirst ++ coalitionSecond).toSet)
                  .exists(_._2 >= characteristicFunction(coalitionFirst) + characteristicFunction(coalitionSecond))
                (superadditive, if (superadditive) coalitionPair else List(), if (superadditive) List() else coalitionPair)
            }
        )

    def isConvex(characteristicFunction: Map[Coalition, Double]): (Boolean, List[List[Coalition]], List[List[Coalition]]) =
        checkCooperativeGameOn(
            characteristicFunction,
            coalitionsPairsFilter = _ => true,
            coalitionsPairsCheckFunction = (coalitionPair: List[Coalition]) => {
                val List(coalitionFirst, coalitionSecond) = coalitionPair
                val unionBenefit = characteristicFunction.find(_._1.toSet == (coalitionFirst ++ coalitionSecond).toSet).map(_._2).getOrElse(0.0)
                val intersectBenefit = characteristicFunction.find(_._1.toSet == coalitionFirst.intersect(coalitionSecond).toSet).map(_._2).getOrElse(0.0)
                val convex = unionBenefit + intersectBenefit >= characteristicFunction(coalitionFirst) + characteristicFunction(coalitionSecond)
                (convex, if (convex) coalitionPair else List(), if (convex) List() else coalitionPair)
            }
        )

    // -------------------------------------------------------------------------------------------------------------
    // * Информационное противоборство
    // -------------------------------------------------------------------------------------------------------------

    private val maxInformationConfrontationIterationsCount = 1000

    @tailrec
    private def isTrustMatrixConvergedHandler(
                                               trustMatricesDiffElements: Array[Double],
                                               epsilon: Double,
                                               converged: Boolean = true
                                             ): Boolean = trustMatricesDiffElements match {
        case Array() => converged
        case _ if !converged => false
        case _ => isTrustMatrixConvergedHandler(
            trustMatricesDiffElements.tail,
            epsilon,
            trustMatricesDiffElements.head < epsilon
        )
    }

    private def isTrustMatrixConverged(prevMatrix: Matrix, currMatrix: Matrix, epsilon: Double): Boolean =
        isTrustMatrixConvergedHandler((currMatrix - prevMatrix).matrix.flatten, epsilon)

    @tailrec
    private def computeTrustMatrix(trustMatrix: Matrix, epsilon: Double, iterationsCount: Int = 1)
                                  (previousTrustMatrix: Matrix = trustMatrix)
                                  (currentTrustMatrix: Matrix = previousTrustMatrix * trustMatrix): Either[String, (Matrix, Int)] = {
        if (isTrustMatrixConverged(previousTrustMatrix, currentTrustMatrix, epsilon)) {
            Right((currentTrustMatrix, iterationsCount))
        } else if (iterationsCount >= maxInformationConfrontationIterationsCount) {
            Left("Матрица не сходится")
        } else {
            computeTrustMatrix(trustMatrix, epsilon, iterationsCount + 1)(currentTrustMatrix)(currentTrustMatrix * trustMatrix)
        }
    }

    def computeInformationConfrontation(
                                         trustMatrix: Matrix,
                                         epsilon: Double,
                                         agentsMinOpinionInitial: Int,
                                         agentsMaxOpinionInitial: Int,
                                         agentsInfluenceMinOpinionInitial: Int,
                                         agentsInfluenceMaxOpinionInitial: Int,
                                         firstGamerAgentsInfluenceCount: Int,
                                         secondGamerAgentsInfluenceCount: Int
                                       ): Either[String, MSA] = {
        computeTrustMatrix(trustMatrix, epsilon)()() map { case (computeTrustMatrix, iterationsCount) =>
            val agentsOpinionsInitialWithoutInfluence = (0 until trustMatrix.getMatrixRowsCount).map { _ =>
                agentsMinOpinionInitial + scala.util.Random.nextInt((agentsMaxOpinionInitial - agentsMinOpinionInitial) + 1)
            }.toList

            println(agentsOpinionsInitialWithoutInfluence.mkString(","))

            val agentsOpinionsResultWithoutInfluence =
                (computeTrustMatrix * Matrix(Array(agentsOpinionsInitialWithoutInfluence.map(_.toDouble).toArray)).transpose).transpose.matrix.head

            var availableInfluencersIndices = (0 until trustMatrix.getMatrixRowsCount).toList
            val firstGamerAgentsInfluenceMinOpinionInitial = agentsInfluenceMinOpinionInitial
            val firstGamerAgentsInfluenceMaxOpinionInitial = agentsInfluenceMaxOpinionInitial
            val firstGamerOpinionInitial = firstGamerAgentsInfluenceMinOpinionInitial + scala.util.Random.nextInt((firstGamerAgentsInfluenceMaxOpinionInitial - firstGamerAgentsInfluenceMinOpinionInitial) + 1)
            val firstGamerInfluencers = (0 until firstGamerAgentsInfluenceCount).map { _ =>
                availableInfluencersIndices(scala.util.Random.nextInt(availableInfluencersIndices.length))
            }.toList

            availableInfluencersIndices = availableInfluencersIndices.filter(availableIndex => !firstGamerInfluencers.contains(availableIndex))
            val secondGamerAgentsInfluenceMinOpinionInitial = if (agentsInfluenceMaxOpinionInitial == 0) agentsInfluenceMaxOpinionInitial else -agentsInfluenceMaxOpinionInitial
            val secondGamerAgentsInfluenceMaxOpinionInitial = if (agentsInfluenceMinOpinionInitial == 0) agentsInfluenceMinOpinionInitial else -agentsInfluenceMinOpinionInitial
            val secondGamerOpinionInitial = secondGamerAgentsInfluenceMinOpinionInitial + scala.util.Random.nextInt((secondGamerAgentsInfluenceMaxOpinionInitial - secondGamerAgentsInfluenceMinOpinionInitial) + 1)
            val secondGamerInfluencers = (0 until secondGamerAgentsInfluenceCount).map { _ =>
                availableInfluencersIndices(scala.util.Random.nextInt(availableInfluencersIndices.length))
            }.toList

            val agentsOpinionsInitialWithInfluence = (
              firstGamerInfluencers.map(influencer => (influencer, firstGamerOpinionInitial)) ++
                secondGamerInfluencers.map(influencer => (influencer, secondGamerOpinionInitial))
              ).foldLeft(agentsOpinionsInitialWithoutInfluence) {
                case (updatedAgentsOpinionsInitialWithoutInfluence, (influencer, opinionInitial)) =>
                    updatedAgentsOpinionsInitialWithoutInfluence.updated(influencer, opinionInitial)
            }

            val agentsOpinionsResultWithInfluence =
                (computeTrustMatrix * Matrix(Array(agentsOpinionsInitialWithInfluence.map(_.toDouble).toArray)).transpose).transpose.matrix.head

            Map(
                "computeTrustMatrix" -> computeTrustMatrix.matrix,
                "agentsOpinionsInitial" -> Map(
                    "withoutInfluence" -> agentsOpinionsInitialWithoutInfluence,
                    "withInfluence" -> agentsOpinionsInitialWithInfluence
                ),
                "agentsOpinionsResult" -> Map(
                    "withoutInfluence" -> agentsOpinionsResultWithoutInfluence,
                    "withInfluence" -> agentsOpinionsResultWithInfluence
                ),
                "iterations" -> iterationsCount,
                "firstGamer" -> Map(
                    "influencers" -> firstGamerInfluencers,
                    "opinionInitial" -> firstGamerOpinionInitial
                ),
                "secondGamer" -> Map(
                    "influencers" -> secondGamerInfluencers,
                    "opinionInitial" -> secondGamerOpinionInitial
                ),
                "winner" -> {
                    if (agentsOpinionsResultWithInfluence.head >= firstGamerAgentsInfluenceMinOpinionInitial) "первый"
                    else "второй"
                }
            )
        }
    }

    // -----------------------------------------------------------------------------------------------------------------
    // Методы работы с таблицами
    // -----------------------------------------------------------------------------------------------------------------

    def formatTable(table: Seq[Seq[Any]]): String = table match {
        case Seq() =>
            ""
        case _ =>
            val sizes = table.map(row => row.map(element =>  if (element == null) 0 else element.toString.length))
            val columnsSizes = sizes.transpose.map(size => size.max)
            val rows = table.map(row => formatRow(row, columnsSizes))
            formatRows(rowSeparator(columnsSizes), rows)
    }

    private def formatRows(rowSeparator: String, rows: Seq[String]): String = {
        (rows.map(row => List(rowSeparator, row).mkString("\n")) :+ rowSeparator).mkString("\n")
    }

    private def formatRow(row: Seq[Any], columnsSizes: Seq[Int]): String = {
        val columns = row.zip(columnsSizes).map { case (element, size) =>
            if (size == 0) "" else ("%" + size + "s").format(element)
        }
        columns.mkString("|", "|", "|")
    }

    private def rowSeparator(columnsSizes: Seq[Int]): String = columnsSizes.map("-" * _).mkString("+", "+", "+")

}

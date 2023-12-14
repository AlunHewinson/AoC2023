package utils

import java.io.File
import java.nio.file.{Files, Paths}

object testFileCreator extends App {

  private def createEmptyTextFiles(year: Int): Unit = {
    (1 to 25).foreach(day => {
      val filePath = s"input/$year/day${day}test.txt"
      val file = new File(filePath)

      try {
        if (!file.exists()) {
          Files.createFile(Paths.get(filePath))
          println(s"Created $year day $day successfully")
        } else {
          println(s"$year day $day already exists")
        }
      } catch {
        case e: Exception => println("An error occurred: " + e.getMessage)
      }
    })
  }
  createEmptyTextFiles(2023)
}

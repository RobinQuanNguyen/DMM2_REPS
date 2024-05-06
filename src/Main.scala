import java.io.PrintWriter
import scala.io.Source
import scala.io.StdIn.readLine
import java.net.{HttpURLConnection, URL}
import java.text.SimpleDateFormat
import java.time.LocalDateTime
import java.util.{Date, Calendar}


object Main {
  def menu(): Int = {
    println("1) Monitor and control energy sources")
    println("2) Get data")
    println("3) View Data")
    println("4) Analyze Data")
    println("0) Exit")
    print("Please enter your choice: ")
    val choice = readLine().toInt
    choice
  }

  def readAndPrintCSV(filePath: String): Unit = {
    val source = Source.fromFile(filePath)
    println("Solar\t|\tWinds\t|\tHydro")
    try {
      for (line <- source.getLines.drop(1)) { // drop the header line
        val cols = line.split(",").map(_.trim)
        println(s"${cols(0)}\t\t\t${cols(1)}\t\t\t${cols(2)}")
      }
    } finally {
      source.close()
    }

    println("Do you want to modify the value? 1: Yes, 2: No")
    val choice = readLine().toInt
    if (choice == 1) {
      modifyValueCSV(filePath) // Call the function to modify the value
    }
  }

  // Function to modify the value in the CSV file
  def modifyValueCSV(filePath: String): Unit = {
    try {
      val source = Source.fromFile(filePath)
      var lines: List[String] = List()
      try {
         lines = source.getLines().toList
      } finally {
        source.close()
      }
      val header = lines.head.split(",")

      println("Please enter the row you want to modify:\n1) Solar\n2) Winds\n3) Hydro\n4) Cancel")
      val row = readLine().toInt

        val columnIndex = row match {
          case 1 => header.indexOf("Solar")
          case 2 => header.indexOf("Winds")
          case 3 => header.indexOf("Hydro")
          case 4 => 0
          case _ => -1
        }

        if (columnIndex != -1) {
          print("Please enter the new value: ")
          val newValue = readLine()

          val updatedLines = lines.tail.map { line =>
            val cols = line.split(",")
            cols(columnIndex) = newValue
            cols.mkString(",")
          }

          val writer = new PrintWriter(filePath)
          try {
            writer.println(lines.head) // write the header
            updatedLines.foreach(writer.println) // write the updated lines
          } finally {
            writer.close()
          }
        } else if (columnIndex == 0) {
          println("Operation cancelled.")
        } else {
          println("Invalid row selection.")
        }
      } catch {
        case e: Exception => println("Something went wrong. Please try again.")
    }

  }

  def getData(): Unit = {
    println("The date of these data is from today to one month ago")

    // get the current date and time
    val atTheMoment = LocalDateTime.now().truncatedTo(java.time.temporal.ChronoUnit.SECONDS)

    val apiSource = Map(
      181 -> "src/winds_data.csv",
      188 -> "src/nuclear_data.csv",
      191 -> "src/hydro_data.csv",
      192 -> "src/total_electricity_data.csv"
    )
    try {
      // Get the data and put them into different files
      for ((x, y) <- apiSource) {
        val url = s"https://data.fingrid.fi/api/datasets/$x/data?startTime=${atTheMoment.minusMonths(1).toString().concat("Z")}&endTime=${atTheMoment.toString().concat("Z")}&format=csv&page=1&pageSize=20000&locale=en&sortBy=startTime&sortOrder=asc"
        val api_key = Source.fromFile("src/api_key.txt").getLines().mkString

        // Connect to the URL and get the response
        val connection = new URL(url).openConnection().asInstanceOf[HttpURLConnection]
        connection.setRequestMethod("GET")
        connection.setRequestProperty("Cache-Control", "no-cache")
        connection.setRequestProperty("Accept", "text/csv")
        connection.setRequestProperty("x-api-key", api_key)

        val res = connection.getResponseCode
        if (res == 200) {
          // Read the response
          val response = Source.fromInputStream(connection.getInputStream).getLines().mkString("\n")

          // Drop the first 9 characters of the response
          val modifiedResponse = response.drop(9)

          // Delete the unnecessary part of the response
          val finalResponse = modifiedResponse.split("\"", 2)(0)

          // Open a PrintWriter to the CSV file
          val writer = new PrintWriter(y)
          try {
            // Split the modified response into lines and write each line to the file
            finalResponse.split("\\\\n").foreach(writer.println)
          } finally {
            // Close the PrintWriter
            writer.close()
          }

          if (x == 181) {
            println("Wind data fetched successfully.")
          } else if (x == 188) {
            println("Nuclear data fetched successfully.")
          } else if (x == 191) {
            println("Hydro data fetched successfully.")
          } else if (x == 192) {
            println("Total electricity data fetched successfully.")
          }
        } else {
          println(s"GET request not worked. Response Code: $res")
        }
      }
    } catch {
      case e: Exception => println("Something went wrong when downloading data. Please try again.")
    }
  }

  //Function 3: view the data. This function includes:
  def handleFile(): List[(String, Double)] = {
    try {
      println("Choose the type of resource that you want to process:\n1) Nuclear\n2) Winds\n3) Hydro\n4) Total Electricity\nEnter your choice: ")
      val choice = scala.io.StdIn.readLine().toInt


      val userChoice = choice match {
        case 1 => "src/nuclear_data.csv"
        case 2 => "src/winds_data.csv"
        case 3 => "src/hydro_data.csv"
        case 4 => "src/total_electricity_data.csv"
        case _ =>  null
      }
      val source = Source.fromFile(userChoice)
      val data = source.getLines().drop(1).map { line =>
        val cols = line.split(",")
        (cols(1), cols(3).toDouble)
      }.toList
      source.close
      data

    } catch {
      case e: Exception => List()
    }
  }

  // Function to filter the data. It is the most important function in this program, because all the main functions are put in this function.
  def filterData(data: List[(String, Double)], mode: Int): Unit = {
    if (data.isEmpty) {
      println("No data available.")
      return
    }

    try {
      val format = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'")
      val dayFormat = new SimpleDateFormat("yyyy-MM-dd")

      print("1) Data for the last hour\n2) Data for the last day\n3) Data for the last week\n4) Data for the last month\n5) Data for a specific day\nEnter your choice: ")
      val choice = scala.io.StdIn.readLine().toInt

      val lastLine = data.last
      val (lastDateStr, lastValue) = lastLine

      val lastDate = format.parse(lastDateStr)
      val lastCal = Calendar.getInstance()
      lastCal.setTime(lastDate)
      val lastHour = lastCal.get(Calendar.HOUR_OF_DAY)
      val lastDay = lastCal.get(Calendar.DAY_OF_YEAR)
        var valueList = List[Double]()
      var dateAndValueList = List[(String, Double)]()

      if (choice == 1) {
        // Get the data for the last hour in the file
        data.foreach { case (timeStr, value) =>
          val time = format.parse(timeStr)
          val cal = Calendar.getInstance()
          cal.setTime(time)
          val hour = cal.get(Calendar.HOUR_OF_DAY)
          val day = cal.get(Calendar.DAY_OF_YEAR)

          // Check the condition to make sure that the requirement is fullfilled
          if (hour == lastHour && day == lastDay) {
            dateAndValueList = (timeStr, value) :: dateAndValueList    // Add the data(date and value) to the list
            valueList = value :: valueList // Add the value to the list
          }
        }
        // Mode 1: Main function 3, mainly for viewing data
        if (mode == 1) {
          val sortedData = sortFunction(dateAndValueList)
          sortedData.foreach { case (time, value) =>
            println(s"Time: $time, Value: $value")
          }
          dateAndValueList = List() // Empty the list

        // Mode 2: Main function 4, mainly for analyzing data
        } else if (mode == 2) {
          dataCalculator(valueList)
          valueList = List() // Empty the list
        }

      } else if (choice == 2) {
        // Get the data for the last day in the file
        data.foreach { case (timeStr, value) =>
          val time = format.parse(timeStr)
          val cal = Calendar.getInstance()
          cal.setTime(time)
          val day = cal.get(Calendar.DAY_OF_YEAR)

          if (day == lastDay) {
            dateAndValueList = (timeStr, value) :: dateAndValueList // Add the data(date and value) to the list
            valueList = value :: valueList // Add the value to the list
          }
        }
        // Mode 1: Main function 3, mainly for viewing data
        if (mode == 1) {
          val sortedData = sortFunction(dateAndValueList)
          sortedData.foreach { case (time, value) =>
            println(s"Time: $time, Value: $value")
          }
          dateAndValueList = List() // Empty the list

          // Mode 2: Main function 4, mainly for analyzing data
        } else if (mode == 2) {
          dataCalculator(valueList)
          valueList = List() // Empty the list
        }
      } else if (choice == 3) {
        // Get the data for the last week in the file
        data.foreach { case (timeStr, value) =>
          val time = format.parse(timeStr)
          val cal = Calendar.getInstance()
          cal.setTime(time)
          val day = cal.get(Calendar.DAY_OF_YEAR)

          if (day >= lastDay - 7) {
            dateAndValueList = (timeStr, value) :: dateAndValueList // Add the data(date and value) to the list
            valueList = value :: valueList // Add the value to the list
          }
        }
        // Mode 1: Main function 3, mainly for viewing data
        if (mode == 1) {
          val sortedData = sortFunction(dateAndValueList)
          sortedData.foreach { case (time, value) =>
            println(s"Time: $time, Value: $value")
          }
          dateAndValueList = List() // Empty the list

          // Mode 2: Main function 4, mainly for analyzing data
        } else if (mode == 2) {
          dataCalculator(valueList)
          valueList = List() // Empty the list
        }
      } else if (choice == 4) {
        // Get the data for the last month in the file
        data.foreach { case (timeStr, value) =>
          val time = format.parse(timeStr)
          val cal = Calendar.getInstance()
          cal.setTime(time)
          val day = cal.get(Calendar.DAY_OF_YEAR)

          if (day >= lastDay - 30) {
            dateAndValueList = (timeStr, value) :: dateAndValueList // Add the data(date and value) to the list
            valueList = value :: valueList // Add the value to the list
          }
        }
        // Mode 1: Main function 3, mainly for viewing data
        if (mode == 1) {
          val sortedData = sortFunction(dateAndValueList)
          sortedData.foreach { case (time, value) =>
            println(s"Time: $time, Value: $value")
          }
          dateAndValueList = List() // Empty the list

          // Mode 2: Main function 4, mainly for analyzing data
        } else if (mode == 2) {
          dataCalculator(valueList)
          valueList = List() // Empty the list
        }
      } else if (choice == 5) {
        // Search for a particular date
        val oldestDateStr = data.minBy(_._1)._1
        val oldestDate = format.parse(oldestDateStr)


        println("Please enter a date in the format yyyy-MM-dd")
        val userInput = readLine()

        if (userInput.matches("\\d{4}-\\d{2}-\\d{2}")) {
          val specificDay = dayFormat.parse(userInput)

          // Check input date
          if (specificDay.before(oldestDate)) {
            println("The date you entered is before the oldest date in the file.")
            return
          }

          val nextDay = Calendar.getInstance()
          nextDay.setTime(specificDay)
          nextDay.add(Calendar.DATE, 1)
          val filteredData = data.filter { case (time, _) =>
            val date = format.parse(time)
            date.after(specificDay) && date.before(nextDay.getTime)
          }

          // Mode 1: Main function 3, mainly for viewing data
          if (mode == 1) {
            filteredData.foreach { case (time, value) =>
              dateAndValueList = (time, value) :: dateAndValueList // Add the data to the list
            }
            val sortedData = sortFunction(dateAndValueList) // get the sorted data
            sortedData.foreach { case (time, value) =>
              println(s"Time: $time, Value: $value")
            }
            dateAndValueList = List() // Empty the list

            // Mode 2: Main function 4, mainly for analyzing data
          } else if (mode == 2) {
            filteredData.foreach { case (time, value) =>
              valueList = value :: valueList
            }
              dataCalculator(valueList)
              valueList = List() // Empty the list
          }

        } else {
          println("Invalid input. Please enter a date in the format yyyy-MM-dd.")
        }}
    } catch {
        case e: Exception => println("Something went wrong. Please try again.")
    }

  }

  // Function to alert the user when the energy is too low or too high
  def lowEngergyAlert(): Unit = {
    try {
      val apiSource = Map(
        181 -> "src/winds_data.csv",
        188 -> "src/nuclear_data.csv",
        191 -> "src/hydro_data.csv",
        192 -> "src/total_electricity_data.csv"
      )

      for ((_, filePath) <- apiSource) {
        val source = scala.io.Source.fromFile(filePath)
        try {
          for (line <- source.getLines.drop(1)) { // drop the header line
            val cols = line.split(",").map(_.trim)
            val value = cols(3).toDouble
            if (value < 50 || value > 15000) {
              println(s"-*- Alert! File: $filePath, Date: ${cols(1)}. The data is not normal: $value MW. Please intervene to bring the data back to normal levels.")
            }
          }
        } finally {
          source.close()
        }
      }
    } catch {
      case e: Exception => null
    }
  }

  // Function to calculate the mean, median, mode, range, and midrange
  def dataCalculator(data: List[(Double)]): Unit = {
    if (data == null) {
      println("No data available.")
      return
    }

    // Calculate the mean/the average
    val mean = data.sum / data.length
    println(f"Mean: $mean%.3f MW")

    // Calculate the median
    val sortedData = data.sorted
    val middle = sortedData.length / 2  // find the middle of the list

    val median = if (sortedData.length % 2 == 1) {
      sortedData(middle)
    } else {
      val (up, down) = sortedData.splitAt(middle)
      (up.last + down.head) / 2.0
    }
    println(f"Median: $median%.3f MW")

    // Calculate the mode
    val counts = data.groupBy(identity).view.mapValues(_.size).toMap
    val mode = counts.maxBy(_._2)._1
    println(f"Mode: $mode%.3f MW")

    // Calculate the range
    var min, max = 0.0

    for (value <- data) {
      if (value < min) {
        min = value
      }
      if (value > max) {
        max = value
      }
    }
    val range = max - min
    println(f"Range: $range%.3f MW")

    // Calculate the midrange
    val midrange = (min + max) / 2
    println(f"Midrange: $midrange%.3f MW")
  }

  // Function to sort the data
  def sortFunction(data: List[(String, Double)]): List[(String, Double)] = {
    print("1) Sort by time\n2) Sort by value\nEnter your choice: ")
    val mode = scala.io.StdIn.readLine().toInt
    if (mode == 1) {
      // Sort by date
      val sortedByDate = data.sortWith((a, b) => a._1 < b._1)
      sortedByDate
    } else {
      // Sort by value
      val sortedByValue = data.sortWith((a, b) => a._2 < b._2)
      sortedByValue
    }
  }


  def main(args: Array[String]): Unit = {
    val filePath = "src/DataResource.csv"

    while (true) {
      try {
        lowEngergyAlert() // Always turn on the alert

        val userChoice = menu() // Display menu and get user choice
        if (userChoice == 1) {
          readAndPrintCSV(filePath)

        } else if (userChoice == 2) {
          getData()

        } else if (userChoice == 3) {
          val data = handleFile()
          filterData(data, 1)

        } else if (userChoice == 4) {
          val data = handleFile()
          filterData(data, 2)

        }else if (userChoice == 0) {
          println("Goodbye!")
          System.exit(0)

        } else {
          println("Invalid choice. Please try again.")
        }
      } catch {
        case e: Exception => println("Something went wrong. Please try again.")
      }
    }
  }
}


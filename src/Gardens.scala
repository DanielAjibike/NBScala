/**
  * Created by Daniel on 14/06/2016.
  */
object Gardens {

  def orderCheck(){

  }

  def main(args: Array[String]): Unit = {

    var menuNo = true


    val order1 = new Orders("001", "4295", "John Doe", "12 Ivy Lane, Brixton, London, SE4 3ER", "5 John Snow Gnomes, 1 Garden Hose", "Yes")
    val order2 = new Orders("002", "4756", "Jane Doe", "21 Borough Road, Walworth, London, SE17 4PQ", "5 Justin Bieber Gnomes", "Yes")
    val order3 = new Orders("003", "4822", "Isaac Mathis", "208 Milky Way, Enfield, London, E45 6QZ", "4 Garden Hoses", "No")
    val order4 = new Orders("004", "4907", "Oliver Queen", "TheHeart, MediaCityUK, Manchester, M50 2TJ", "10 Rakes", "Yes")
    val order5 = new Orders("005", "4988", "O'shea Jackson", "12 Lordship Lane, Clapham, London, SE18 9RG", "7 Shovels", "No")

    val orderList = Array(order1, order2, order3, order4, order5)

    val status1 = new OrderStatus("001", "Not Processed")
    val status2 = new OrderStatus("002", "Not Processed")
    val status3 = new OrderStatus("003", "Not Processed")
    val status4 = new OrderStatus("004", "Not Processed")
    val status5 = new OrderStatus("005", "Not Processed")

    val statusList = Array(status1, status2, status3, status4, status5)

    val location1 = new ItemLocation("A652", "B", "C6", "R26" )
    val location2 = new ItemLocation("F150", "D", "G8", "R3" )
    val location3 = new ItemLocation("G611", "E", "S1", "R6" )
    val location4 = new ItemLocation("HK25", "F", "B12", "R12" )
    val location5 = new ItemLocation("ZX45", "A", "A4", "R5" )

    val locList = Array(location1, location2, location3, location4, location5)

    def getInput(): String = {
      print("Enter a value: ")
      scala.io.StdIn.readLine()
    }

    while (menuNo) {
      println("Warehouse Management System")
      println()
      println("1. View specific order")
      println("2. Update order status")
      println("3. View order status")
      println("4. Add stock delivery")
      println("5. Locate an item")
      println()
      println("Press Q to quit")
      println()

      getInput() match {
        case "1" =>
          println("Here is a list of the current orders: \n" )
          for (i <- 0 to orderList.length - 1) {
            println("Order " + statusList(i).orderID)
          }
          println()


          print("Enter ID to view item: ")
          val orderID_ = scala.io.StdIn.readLine()

          checkOrder(orderList)

          def checkOrder(ordID: Array[Orders]): Unit = {
            if (ordID.isEmpty) {

            }else if(ordID.head.orderID == orderID_) {
              println()
              println("Order ID: " + ordID.head.orderID)
              println("Customer ID: " + ordID.head.customerID)
              println("Customer Name: " + ordID.head.customerName)
              println("Customer Address: " + ordID.head.customerAddress)
              println("Item Details: " + ordID.head.itemDetails)
              println("Requires porous: " + ordID.head.requirePorous)
              println()
              checkOrder(ordID.tail)
            }else{
              checkOrder(ordID.tail)
            }
            }

        case "2" =>
          print("Which order would you like to update? ")
          val orderID_ = scala.io.StdIn.readLine()
          print("What is your employee ID: ")
          val employeeID_ = scala.io.StdIn.readLine()
          print("Assign status to order: 1. In Progress 2. Processed 3. Dispatched 4. Delivered: ")
          val orderStatus_ = scala.io.StdIn.readLine()


          if(checkStatus(orderID_, orderStatus_)) {
            for (i <- 0 to orderList.length - 1) {
              if (statusList(i).orderID == orderID_) {
                if (orderStatus_ == "1") {
                  statusList(i).orderStatus = "In Progress"
                }
                if (orderStatus_ == "2") {
                  statusList(i).orderStatus = "Processed"
                }
                if (orderStatus_ == "3") {
                  statusList(i).orderStatus = "Dispatched"
                }
                if (orderStatus_ == "4") {
                  statusList(i).orderStatus = "Delivered"
                }
                println("Employee " + employeeID_ + " has changed order " + orderID_ + " status to " + statusList(i).orderStatus)
              }
            }
          }else {
              println("That is not a valid option!")
            }

        case "3" =>
          println()
          println("Here is the full list of orders: ")

          for (i <- 0 to orderList.length - 1) {
            print("Order ID " + statusList(i).orderID + " has the status: " + statusList(i).orderStatus)
            println()
          }

          println()

        case "4" =>
          print("Which item would you like to add to the inventory: ")
          val addItem_ = scala.io.StdIn.readLine()
          println()

          print("How many " + addItem_ + "(s) would you like to add to the inventory: ")
          val itemQuantity_ = scala.io.StdIn.readLine()
          println()

          println("You added " + itemQuantity_ + " " + addItem_ + "(s) to the inventory")
          println()

        case "5" =>
          print("Enter ID of item you would like to locate: ")
          val itemID_ = scala.io.StdIn.readLine()
          println()

          if(checkLocation(itemID_)) {
            for (i <- 0 to locList.length - 1) {
              if (locList(i).itemID == itemID_) {
                println("Item " + itemID_ + " is located in")
                println("Warehouse Block: " + locList(i).wareBlock)
                println("Shelf: " + locList(i).wareShelf)
                println("Row: " + locList(i).wareRow)
                println()

              }

            }
          }else{
            println("This item can not be found!")
          }

        case "Q" =>
          menuNo = false

        case _ =>
          println("Invalid option")

      }
    }

    def checkStatus(orderID_ :String, orderStatus_ :String): Boolean ={
      var csReturn = false

      for (i <- 0 to statusList.length - 1) {
        if (statusList(i).orderID == orderID_) {
          csReturn = true
        }
      }
      csReturn
    }

    def checkLocation(itemID_ :String): Boolean ={
      var locReturn = false

      for(i <- 0 to locList.length - 1 ){
        if(locList(i).itemID == itemID_ ){
          locReturn = true
        }
      }
      locReturn
    }

  }
}
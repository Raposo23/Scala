import java.text.SimpleDateFormat
import java.time.LocalDate
import java.util.{Calendar, Collections, Date}
import scala.collection.mutable.ListBuffer
import scala.util.Random
import scala.util.Random.javaRandomToRandom


object Scala_Exercise{
  private var orders: ListBuffer[Order] = ListBuffer()

  def main(args:Array[String]): Unit ={

    orders = createOrders()

    Filter(orders,"2020-01-01","2021-12-12")


  }

  def Filter(enc: ListBuffer[Order], data_in:String, data_end:String){
    var prodDates: ListBuffer[Date] = ListBuffer()
    prodDates = listOldProd(enc)
    var entrada = new SimpleDateFormat("yyyy-MM-dd").parse(data_in)
    var saida = new SimpleDateFormat("yyyy-MM-dd").parse(data_end)
    var diff = saida.getTime() - entrada.getTime();
    var dias = (diff / (60 * 60 * 24 * 1000));

    assert(dias >= 365,"ERRO: O intervalo tem de ser superior a 1 ano !!!")

    var three_months:Date = addMonth(entrada,3)
    var six_months:Date = addMonth(three_months,3)
    var twelve_months:Date = addMonth(six_months,6)


    var range1:Int  = 0
    var range2:Int  = 0
    var range3:Int  = 0
    var range4:Int  = 0


    for(d <- prodDates){
      if(d.getTime - entrada.getTime >= 0 && d.getTime - three_months.getTime <= 0){
        range1 += 1
      }else if(d.getTime - three_months.getTime > 0 && d.getTime - six_months.getTime <= 0){
        range2 += 1
      }else if(d.getTime - six_months.getTime > 0 && d.getTime - twelve_months.getTime <= 0){
        range3 += 1
      }else if(d.getTime - twelve_months.getTime > 0){
        range4 += 1
      }
    }


    println("\n\n1-3 months " + range1 + " orders\n")
    println("4-6 months " + range2 + " orders\n")
    println("7-12 months " + range3 + " orders\n")
    println("> 12 months " + range4 + " orders\n")
  }

  def addMonth(date:Date, i:Int):Date ={
    var c:Calendar = Calendar.getInstance()
    c.setTime(date)
    c.add(Calendar.MONTH,i)

    return c.getTime()
  }

  def createOrders():ListBuffer[Order] = {
    for(order <- 1 to 400){
      orders.append(new Order())
    }
    return(orders)
  }

  def listOldProd(orders:ListBuffer[Order]): ListBuffer[Date] ={
    var prod = new ListBuffer[Date]
    for(a <- orders){
      prod.append(a.oldDate())
    }
    return prod
  }
}



class Order(val name:String, val contact:String, val address:String, val total:Double, var orderDate: Date, var item:ListBuffer[Item]) {

  def this() = {
    this("order", "912345678","Rua",230.1,new Date,ListBuffer())
    this.orderDate = randomDate()
    this.item = randItems()
  }

  //  override def toString: String = "[ ORDER: " + orderDate + " PRODUCT: " + item + "]\n"

  override def toString:String = {
    var item2:String = "\n[ ORDER: " + orderDate + " PRODUCT: "
    for(items <- item){
      item2 = item2 + " | " + items
    }
    return item2 + "]\n"
  }

  def randomDate(): Date = {
    val inicio = new SimpleDateFormat("yyyy-MM-dd").parse("2015-01-01 00:00:00").getTime()
    val fim = new SimpleDateFormat("yyyy-MM-dd").parse("2022-11-12 00:00:00").getTime()
    val rand = Random.between(inicio, fim)

    return new Date(rand)
  }

  def randItems(): ListBuffer[Item] = {
    var itemList: ListBuffer[Item] = ListBuffer()
    for(count <- 1 to (new scala.util.Random().nextInt(3)+1)){
      var item = new Item()
      itemList.append(item)
    }
    return itemList
  }

  def oldDate():Date = {
    val format = new SimpleDateFormat("yyyy-MM-dd")
    var old:Date = format.parse("2700-12-31")
    for(items <- item){
      if(old.compareTo(items.product.creationDate) > 0){
        old = items.product.creationDate
      }
    }
    return(old)
  }


}

class Item(val cost:Double,val shipping:Double,val tax:Double, var product: Product){
  def this() {
    this(1.0,2.0,2.3,new Product())
  }

  override def toString:String = ""+product+""

}

class Product(val name:String,val category:String,val weight:Double, val price:Double, var creationDate:Date  ){
  def this()={
    this("PC","Tech",1.0,3.0,new Date())
    this.creationDate = randomDate()
  }

  //override def toString:String = ""+name+""
  override def toString():String = "["+name+"|"+creationDate+"]"

  def randomDate(): Date = {
    val inicio = new SimpleDateFormat("yyyy-MM-dd").parse("2020-01-01 00:00:00").getTime()
    val fim = (new Date().getTime())
    val rand = Random.between(inicio, fim)

    return new Date(rand)
  }

}

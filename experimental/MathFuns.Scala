package datatable_math

import com.github.martincooper.datatable._;
import scala.util.Try



class MathFuns {
def sum(table: DataTable, colname: String): Double = {
  //0.0 used to force ans to be double to avoid type issues
  var ans = 0.0;
 table.foreach(dataRow => ans = ans+dataRow.as[Double](colname))
  return ans;
}
def avg(table :DataTable, colname :String): Double = {
  var avg = sum(table,colname)/table.length;
  return avg;
}





}

package no.aksjeape

import org.scalatest.FunSuite
import scalaj.http.{HttpOptions, Http}
import scalaj.http.Http.Request

class YQLQuoteTest extends FunSuite {
    val targetUrl = "https://query.yahooapis.com/v1/public/yql"
    test("YQL search for quotes work") {
        val query = "select * from csv where url='http://download.finance.yahoo.com/d/quotes.csv?s=YHOO,GOOG,AAPL&f=sl1d1t1c1ohgv&e=.csv' and columns='symbol,price,date,time,change,col1,high,low,col2'"

        val queryRequest: Request = Http(targetUrl).param("q", query).option(HttpOptions.connTimeout(5000))
        assert(queryRequest.responseCode == 200)
    }
}

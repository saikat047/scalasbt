package no.aksjeape

import scala.collection.JavaConverters._
import org.scalatest.FunSuite
import scalaj.http.{HttpOptions, Http}
import scalaj.http.Http.Request
import org.htmlcleaner.{TagNode, CleanerProperties, HtmlCleaner}

class InformationFromBloomBergTest extends FunSuite {
    test("Information from Bloomberg is retrieved and printed") {
        val listOfCompaniesReq: Request = Http("http://www.bloomberg.com/markets/companies/country/sweden/")
            .option(HttpOptions.connTimeout(3000))
            .option(HttpOptions.readTimeout(1000))
        assert(listOfCompaniesReq.responseCode == 200)

        val listOfCompanies: String = listOfCompaniesReq.asString

        val htmlCleaner = createHtmlCleaner
        val listOfCompaniesNode: TagNode = htmlCleaner.clean(listOfCompanies)

        val bottomPaginationNode: TagNode = listOfCompaniesNode
            .getAllElements(true)
            .filter(node => Option(node.getAttributeByName("class")).getOrElse("").contains("pagination"))
            .head

        assert(bottomPaginationNode != null)
        val paginationLinks: Array[TagNode] = bottomPaginationNode.getElementsByName("a", false).filter(node => node.getAttributeByName("class") == null)
        assert(paginationLinks != null)
        assert(!paginationLinks.isEmpty)

        paginationLinks.map(_.getAttributeByName("href")).foreach(println)

        val tickerDataNode: TagNode = listOfCompaniesNode.findElementByAttValue("class", "ticker_data", true, false)

        assert(tickerDataNode.getName == "table")

        val tickerDataNodeBody: TagNode = tickerDataNode.getElementsByName("tbody", false)(0)
        val companyRows = tickerDataNodeBody.getElementsByName("tr", false).tail
        assert(!companyRows.isEmpty)

        val companySymbolList: Array[(TagNode, StringBuffer)] = companyRows.map(node =>
            (node.getElementsByAttValue("class", "name", false, false)(0).getElementsByName("a", false)(0),
            node.getElementsByAttValue("class", "symbol", false, false)(0).getText)
        )
        companySymbolList.foreach(value => println(value._1.getAttributeByName("href") + ", " + value._2))
    }

    def createHtmlCleaner : HtmlCleaner = {
        val htmlCleaner = new HtmlCleaner
        val cleanerProp: CleanerProperties = htmlCleaner.getProperties
        cleanerProp.setNamespacesAware(false)
        htmlCleaner
    }
}

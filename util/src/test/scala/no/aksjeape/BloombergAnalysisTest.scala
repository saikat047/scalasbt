package no.aksjeape

import org.scalatest.{FunSuite, FunSpec}
import org.htmlcleaner.{TagNode, CleanerProperties, HtmlCleaner}
import scalaj.http.Http.Request
import scalaj.http.{HttpOptions, Http}
import java.net.URI

class BloombergAnalysisTest extends FunSuite {

    val connTimeout = 3000
    val responseTimeout = 10000
    val companiesUrl = "http://www.bloomberg.com/markets/companies/country/sweden/"


    def pageContent(url:String):String = {
        val urlReq: Request = Http(url).option(HttpOptions.connTimeout(connTimeout)).option(HttpOptions.readTimeout(responseTimeout))
        assert(urlReq.responseCode == 200, () => println("url: " + urlReq))
        urlReq.asString
    }

    def companyPages(companiesUrl:String):Array[String] = {
        val listOfCompanies: String = pageContent(companiesUrl)

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
        val links: Array[String] = paginationLinks.map(value => new URI(companiesUrl).resolve(value.getAttributeByName("href")).toString)
        links :+ companiesUrl
    }

    /**
     *
     * @param companiesUrl
     * @return array of company links
     */
    def companyList(companiesUrl:String):Array[String] = {
        val htmlCleaner = createHtmlCleaner
        val listOfCompaniesNode: TagNode = htmlCleaner.clean(pageContent(companiesUrl))

        val bottomPaginationNode: TagNode = listOfCompaniesNode
            .getAllElements(true)
            .filter(node => Option(node.getAttributeByName("class")).getOrElse("").contains("pagination"))
            .head

        assert(bottomPaginationNode != null)
        val paginationLinks: Array[TagNode] = bottomPaginationNode.getElementsByName("a", false).filter(node => node.getAttributeByName("class") == null)
        assert(paginationLinks != null)
        assert(!paginationLinks.isEmpty)

        val tickerDataNode: TagNode = listOfCompaniesNode.findElementByAttValue("class", "ticker_data", true, false)

        assert(tickerDataNode.getName == "table")

        val tickerDataNodeBody: TagNode = tickerDataNode.getElementsByName("tbody", false)(0)
        val companyRows = tickerDataNodeBody.getElementsByName("tr", false).tail
        assert(!companyRows.isEmpty)

        val companySymbolList: Array[(TagNode, StringBuffer)] = companyRows.map(node =>
            (node.getElementsByAttValue("class", "name", false, false)(0).getElementsByName("a", false)(0),
                node.getElementsByAttValue("class", "symbol", false, false)(0).getText)
        )
        companySymbolList.map(value => new URI(companiesUrl).resolve(value._1.getAttributeByName("href")).toString)
    }

    def companyPERatio(companyUrl:String):Double = {

        println("Retrieving company information for '" + companyUrl + "'")

        val htmlCleaner = createHtmlCleaner
        val companyPageNode: TagNode = htmlCleaner.clean(pageContent(companyUrl))

        val keyStatTableList: Array[TagNode] = companyPageNode.getElementsByAttValue("class", "key_stat_data", true, false)
        if (keyStatTableList.isEmpty) {
            return -1.0
        }
        val keyStatTable: TagNode = keyStatTableList(0).getElementsByName("tbody", false)(0)
        assert(keyStatTable != null)

        val statRows: Array[TagNode] = keyStatTable.getElementsByName("tr", false)
        assert(!statRows.isEmpty)

        val companyStats = statRows.map(node => (node.findElementByName("th", false).getText.toString, node.findElementByAttValue("class", "company_stat", false, false).getText.toString))
        assert(!companyStats.isEmpty)

        val peRatioList = companyStats.filter(value => "Current P/E Ratio (ttm)".equalsIgnoreCase(value._1))
        val peRatio = peRatioList.map(value => value._2).head
        assert(peRatio != null)
        try {
            peRatio.toDouble
        } catch {
            case _ => -1.0
        }
    }

    test("Dumps PE-ratio from bloomberg") {
        val urlToCompaniesPages: Array[String] = companyPages(companiesUrl)
        assert(!urlToCompaniesPages.isEmpty)

        val companyLinks: Array[String] = urlToCompaniesPages.map(value => companyList(value)).flatten
        assert(!companyLinks.isEmpty)

        val peSortedCompanies: Array[(String, Double)] = companyLinks.map(value => (value, companyPERatio(value))).sortWith(_._2 < _._2)
        peSortedCompanies.foreach(value => println(value._1 + " = " + value._2))
    }


    def createHtmlCleaner : HtmlCleaner = {
        val htmlCleaner = new HtmlCleaner
        val cleanerProp: CleanerProperties = htmlCleaner.getProperties
        cleanerProp.setNamespacesAware(false)
        htmlCleaner
    }
}

package no.aksjeape

import java.io.{File, PrintWriter, FileWriter}

import org.scalatest.FunSuite
import org.htmlcleaner.{TagNode, CleanerProperties, HtmlCleaner}
import scalaj.http.Http.Request
import scalaj.http.{HttpOptions, Http}
import java.net.URI

class BloombergAnalysisTest extends FunSuite {

    val connTimeout = 20000
    val readTimeout = 20000
    val companiesUrl = "http://www.bloomberg.com/markets/companies/country/norway/"


    def pageContent(url:String):String = {
        val urlReq: Request = Http(url)
            .option(HttpOptions.connTimeout(connTimeout))
            .option(HttpOptions.readTimeout(readTimeout))
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

    def companyPERatioOgDividend(companyUrl:String):Option[(Double, Double, String, String, String)] = {
        val htmlCleaner = createHtmlCleaner
        val companyPageNode: TagNode = htmlCleaner.clean(pageContent(companyUrl))

        val keyStatTableList: Array[TagNode] = companyPageNode.getElementsByAttValue("class", "key_stat_data", true, false)
        if (keyStatTableList.isEmpty) {
            return Option.empty
        }
        val keyStatTableOpt: Option[TagNode] = keyStatTable(companyPageNode)

        val companyStats:Array[(String, String)] = keyStatTableOpt match {
            case Some(table) => statistics(table)
            case None => Array[(String, String)]()
        }

        val peRatio: Double = statisticsValue(companyStats, "Current P/E Ratio") match {
            case Some(ratio) => ratio.toDouble
            case None => -1
        }
        val estimatedPeRatio: Double = statisticsValue(companyStats, "Estimated P/E") match {
            case Some(ratio) => ratio.toDouble
            case None => 0
        }
        val estimatedEps: String = statisticsValue(companyStats, "Est. EPS") match {
            case Some(eps) => eps
            case None => ""
        }
        val cashDividend: String = statisticsValue(companyStats, "Cash Dividend") match {
            case Some(dividend) => dividend
            case None => ""
        }
        val dividendGrossYield: String = statisticsValue(companyStats, "Dividend Indicated Gross Yield") match {
            case Some(value) => value
            case None => ""
        }
        // We have P/E Ratio information and we do not know the expected P/E ratio either.
        if ((peRatio < 0 && estimatedPeRatio == 0) || cashDividend.isEmpty || estimatedEps.isEmpty) {
            Option.empty
        } else {
            Some((peRatio, estimatedPeRatio, estimatedEps, cashDividend, dividendGrossYield))
        }
    }

    def statisticsValue(values:Array[(String, String)], name:String):Option[String] = {
        values
            .find(value => value._1.startsWith(name))
            .map(_._2)
            .find(value => !value.isEmpty && !value.contentEquals("-"))
    }

    def statistics(statisticsTable:TagNode):Array[(String, String)] = {
        val statRows: Array[TagNode] = statisticsTable.getElementsByName("tr", false)
        assert(!statRows.isEmpty)

        val companyStats = statRows.map(node =>
            (
                node.findElementByName("th", false).getText.toString,
                node.findElementByAttValue("class", "company_stat", false, false).getText.toString
            )
        )
        assert(!companyStats.isEmpty)
        return companyStats
    }

    test("Dumps PE-ratio from bloomberg") {
        val urlToCompaniesPages: Array[String] = companyPages(companiesUrl)
        assert(!urlToCompaniesPages.isEmpty)

        val companyLinks: Array[String] = urlToCompaniesPages.map(value => companyList(value)).flatten
        assert(!companyLinks.isEmpty)

        // val companyLinks = Array("http://www.bloomberg.com/quote/NRS:NO")
        val infoCsv = new PrintWriter(new FileWriter(new File(java.lang.System.getProperty("user.home"), "aksjeinfo.csv")))
        infoCsv.println("Url, P/E Ratio, Estimated P/E, Estimated EPS, Cash Dividend,Dividend Indicated Gross Yield")

        companyLinks.foreach(companyLink =>  {
            companyPERatioOgDividend(companyLink) match {
                case Some((peRatio, estimatedPeRatio, estimatedEps, cashDividend, dividendGrossYield)) => {
                    println("Generating aksje informasjon for " + companyLink)
                    infoCsv.println(java.lang.String.format("%s,%s,%s,%s,%s,%s",
                        companyLink, peRatio.toString, estimatedPeRatio.toString, estimatedEps, cashDividend, dividendGrossYield))
                    infoCsv.flush()
                }
                case None => {}
            }
        })
        infoCsv.close()
    }

    def keyStatTable(companyPageNode:TagNode):Option[TagNode] = {
        val keyStatTableList: Array[TagNode] = companyPageNode.getElementsByAttValue("class", "key_stat_data", true, false)
        if (keyStatTableList.isEmpty) {
            return Option.empty
        }
        val keyStatTable: TagNode = keyStatTableList(0).getElementsByName("tbody", false)(0)
        assert(keyStatTable != null)
        Some(keyStatTable)
    }

    def createHtmlCleaner : HtmlCleaner = {
        val htmlCleaner = new HtmlCleaner
        val cleanerProp: CleanerProperties = htmlCleaner.getProperties
        cleanerProp.setNamespacesAware(false)
        htmlCleaner
    }
}

package no.aksjeape

import org.scalatest.FunSuite
import scalaj.http.Http.Request
import scalaj.http.{HttpOptions, Http}
import org.htmlcleaner.{CleanerProperties, HtmlCleaner, TagNode}

class BloombergPERatioTest extends FunSuite {
    test("We can find PE-ratio on Bloomberg company page") {
        val companyPage: Request = Http("http://www.bloomberg.com/quote/NRS:NO")
            .option(HttpOptions.connTimeout(3000))
            .option(HttpOptions.readTimeout(1000))

        assert(companyPage.responseCode == 200)
        val htmlCleaner = createHtmlCleaner
        val companyPageNode: TagNode = htmlCleaner.clean(companyPage.asString)

        val keyStatTable: TagNode = companyPageNode.getElementsByAttValue("class", "key_stat_data", true, false)(0).getElementsByName("tbody", false)(0)
        assert(keyStatTable != null)

        val statRows: Array[TagNode] = keyStatTable.getElementsByName("tr", false)
        assert(!statRows.isEmpty)

        val companyStats = statRows.map(node => (node.findElementByName("th", false).getText.toString, node.findElementByAttValue("class", "company_stat", false, false).getText.toString))
        assert(!companyStats.isEmpty)

        companyStats.foreach(value => println("xxx" + value._1 + "xxx"))

        val peRatioList = companyStats.filter(value => "Current P/E Ratio (ttm)".equalsIgnoreCase(value._1))
        val peRatio = peRatioList.map(value => value._2).head
        assert(peRatio != null)
        println("PE ratio : " + peRatio)
    }

    def createHtmlCleaner : HtmlCleaner = {
        val htmlCleaner = new HtmlCleaner
        val cleanerProp: CleanerProperties = htmlCleaner.getProperties
        cleanerProp.setNamespacesAware(false)
        htmlCleaner
    }
}

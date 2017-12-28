<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="html" indent="yes"/>
  <xsl:template match="/">
    <html>
      <body>
        DuckDuckGo
        <xsl:for-each select="//tr[not(@class) or @class!='result-sponsored']">
          <xsl:for-each select="td/a[@class='result-link']">
            <br />
            <br />
            <a href="{@href}">
              <xsl:value-of select="." />
            </a>
          </xsl:for-each>
          <xsl:for-each select="td[@class='result-snippet']">
            <br />
            <xsl:value-of select="." />
          </xsl:for-each>
        </xsl:for-each>
      </body>
    </html>
  </xsl:template>
</xsl:stylesheet>

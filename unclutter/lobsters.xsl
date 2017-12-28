<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="html" indent="yes"/>
  <xsl:template match="/">
    <html>
      <body>
        Lobsters
        <xsl:for-each select="//div[@class='details']">
          <p>
            <xsl:copy-of select="span[@class='link']/a" />
            <br/>
            <xsl:value-of select="div[@class='byline']/span[position()=1]" />
            |
            <xsl:copy-of select="div[@class='byline']/span[@class='comments_label']/a" />
          </p>
        </xsl:for-each>
      </body>
    </html>
  </xsl:template>
</xsl:stylesheet>

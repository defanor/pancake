<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="html" indent="yes"/>
  <xsl:template match="/">
    <html>
      <body>
        Hacker News
        <xsl:for-each select="//td[@class='title' or @class='subtext']">
          <xsl:choose>
            <xsl:when test="@class = 'title'">
              <xsl:variable name="uri" select="a/@href"/>
              <xsl:if test="$uri!=''">
                <br />
                <br />
                <a href="{$uri}" >
                  <xsl:value-of select="a" />
                </a>
              </xsl:if>
            </xsl:when>
            <xsl:when test="@class = 'subtext'">
              <br/>
              <xsl:value-of select="span[@class='age']" />
              |
              <a href="{a[last()]/@href}">
                <xsl:value-of select="a[last()]" />
              </a>
            </xsl:when>
          </xsl:choose>
        </xsl:for-each>
      </body>
    </html>
  </xsl:template>
</xsl:stylesheet>

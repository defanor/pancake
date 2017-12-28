<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="html" indent="yes"/>

  <xsl:template match="/">
    <html>
      <body>
        <xsl:apply-templates select="//div[@id='content']" />
      </body>
    </html>
  </xsl:template>

  <!-- Cut these out -->
  <xsl:template match="span[@class='mw-editsection']" />
  <xsl:template match="ul[@id='page-actions']" />
  <xsl:template match="div[@class='mw-ui-icon mw-ui-icon-element indicator']" />
  <xsl:template match="a[@class='mw-ui-icon mw-ui-icon-element mw-ui-icon-edit-enabled edit-page']" />

  <!-- Catch-all: copy and apply templates -->
  <xsl:template match="node()" priority="0">
    <xsl:copy>
      <xsl:copy-of select="@*" />
      <xsl:apply-templates />
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>

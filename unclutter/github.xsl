<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="html" indent="yes"/>

  <xsl:template match="/">
    <html>
      <body>
        <xsl:apply-templates select="//div[@role='main']" />
      </body>
    </html>
  </xsl:template>

  <xsl:template match="nav[@role='navigation'] | ul[@class='numbers-summary'] | div[@class='repository-lang-stats']">
    <p>
      <xsl:for-each select=".//a">
        <xsl:if test="position()>1">
          |
        </xsl:if>
        <a href="{@href}">
          <xsl:value-of select="." />
        </a>
      </xsl:for-each>
    </p>
  </xsl:template>

  <!-- files -->
  <xsl:template match="div[@class='file-wrap']">
    <div>
      <xsl:for-each select=".//tr[position()>1]">
        <xsl:copy-of select="td[@class='content']//a" />
        (<xsl:copy-of select=".//time-ago" />)
        <br/>
      </xsl:for-each>
    </div>
  </xsl:template>

  <!-- code -->
  <xsl:template match="table[@class='highlight tab-size js-file-line-container']">
    <pre>
      <xsl:for-each select=".//td[@class='blob-code blob-code-inner js-file-line']">
        <xsl:value-of select="." />
        <xsl:text>&#xa;</xsl:text>
      </xsl:for-each>
    </pre>
  </xsl:template>

  <!-- watch, star, fork -->
  <xsl:template match="ul[@class='pagehead-actions']" />
  <!-- signup -->
  <xsl:template match="div[@class='signup-prompt-bg rounded-1']" />
  <!-- languages, duplication -->
  <xsl:template match="div[@class='repository-lang-stats-graph js-toggle-lang-stats']" />
  <!-- misc clutter -->
  <xsl:template match="div[@class='js-socket-channel js-updatable-content']" />
  <xsl:template match="div[@class='mt-3 mb-2 text-center']" />
  <xsl:template match="div[@class='subnav']" />
  <!-- issue filters and such -->
  <xsl:template match="div[@id='js-issues-toolbar']" />
  <!-- registration thing -->
  <xsl:template match="div[@class='px-4']" />
  <!-- another "sign up for free" thing -->
  <xsl:template match="div[@class='flash flash-warn mt-3']" />
  <!-- protips -->
  <xsl:template match="div[@class='protip']" />
  <!-- branch selection, new PR, etc -->
  <xsl:template match="div[@class='file-navigation in-mid-page']" />


  <!-- Catch-all: copy and apply templates -->
  <xsl:template match="node()" priority="0">
    <xsl:copy>
      <xsl:copy-of select="@*" />
      <xsl:apply-templates />
    </xsl:copy>
  </xsl:template>

</xsl:stylesheet>

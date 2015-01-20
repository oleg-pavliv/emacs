<?xml version="1.0" encoding="ISO-8859-1"?>
<!-- <?xml version="1.0" encoding="UTF-8"?> -->
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="xml" version="1.0" encoding="UTF-8" indent="no"/>

  <xsl:template match="ScheduleProvider">
    <xsl:copy><xsl:copy-of select="@*"/><xsl:apply-templates/></xsl:copy>
  </xsl:template>

  <xsl:template match="Content">
    <xsl:copy><xsl:copy-of select="@*"/><xsl:apply-templates/></xsl:copy>
  </xsl:template>

  <xsl:template match="Media">
    <xsl:copy><xsl:copy-of select="@*"/><xsl:apply-templates/></xsl:copy>
  </xsl:template>

  <xsl:template match="EpgDescription">
    <xsl:copy>
      <xsl:copy-of select="@*"/>
      <xsl:apply-templates select="EpgElement">
        <xsl:sort data-type="text" select="@key"/>
      </xsl:apply-templates>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="*">
    <xsl:copy-of select="."/>
  </xsl:template>


</xsl:stylesheet>

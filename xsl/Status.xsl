<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:sch="http://www.entone.com/Schedule"
                xmlns:c="http://www.entone.com/SLAPI/DataTypes/SCHI/PolicyData/Common"
                xmlns:li="http://www.entone.com/SLAPI/DataTypes/SCHI/PolicyData/LiveIngest"
                exclude-result-prefixes="sch c li">

    <xsl:output method="xml" indent="yes"/>
    <!-- <xsl:strip-space elements="*"/> -->

    <xsl:template match="/sch:UpdateSchedule">
        <Status xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                xmlns:xsd="http://www.w3.org/2001/XMLSchema"
                xmlns="http://www.entone.com/Schedule/Status">
            <Task>
                <xsl:copy-of select="//sch:TaskID"/>
                <Result>
                    <AssetName><xsl:value-of select="//c:FixName" /></AssetName>
                    <StartTime><xsl:value-of select="//li:Start" /></StartTime>
                    <StopTime><xsl:value-of select="//li:Stop" /></StopTime>
                    <Status>Finished</Status>
                    <ReturnCode>2</ReturnCode>
                </Result>
            </Task>
        </Status>
    </xsl:template>

</xsl:stylesheet>
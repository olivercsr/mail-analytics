package main

import (
	"fmt"
  "net/http"
  "github.com/gin-gonic/gin"
)

type webApp struct {
  db existDb
}

func (app webApp) queryCount(c *gin.Context) {
  start, end := c.Param("start"), c.Param("end")

  // parseXml(xmlData)

  query := renderXquery()
  result, err := app.db.queryDb(query)
  if err != nil {
    panic(err)
  }
  fmt.Printf("==================================== %s\n", result)
  parseXml(result)

  /*
  viewRenderer := newViewRenderer("views")
  data := make(map[string]string)
  data["title"] = "thetitle2"
  data["start"] = start
  data["end"] = end
  html := viewRenderer.render("queryResult", data)
  */

  // c.IndentedJSON(http.StatusOK, gin.H{"start": start, "end": end})
  // c.Header("Content-type", "text/html; charset=utf-8")
  // c.String(http.StatusOK, html)
  c.HTML(http.StatusOK, "queryResult.tmpl", gin.H{
    "title": "thetitle3",
    "start": start,
    "end": end,
  })
}

func (app webApp) queryRowCount(c *gin.Context) {
  start, end := c.Param("start"), c.Param("end")

  // parseXml(xmlData)

  query := renderXquery()
  result, err := app.db.queryDb(query)
  if err != nil {
    panic(err)
  }
  fmt.Printf("==================================== %s\n", result)
  parseXml(result)

  /*
  viewRenderer := newViewRenderer("views")
  data := make(map[string]string)
  data["title"] = "thetitle2"
  data["start"] = start
  data["end"] = end
  html := viewRenderer.render("queryResult", data)
  */

  // c.IndentedJSON(http.StatusOK, gin.H{"start": start, "end": end})
  // c.Header("Content-type", "text/html; charset=utf-8")
  // c.String(http.StatusOK, html)
  c.HTML(http.StatusOK, "queryResult.tmpl", gin.H{
    "title": "thetitle3",
    "start": start,
    "end": end,
  })
}

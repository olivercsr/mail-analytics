package main

import (
	"fmt"
	"strconv"
  "regexp"
  "net/http"
  "github.com/gin-gonic/gin"
)

type webApp struct {
  db existDb
  router *gin.Engine
}

func NewWebapp(appcfg appConfig, db existDb) webApp {
  app := webApp{
    db: db,
    router: gin.Default(),
  }

  app.router.Use(make_authenticate(
		appcfg.authUserHeader,
		appcfg.devAuthuser,
		))

  app.router.Static("/static", "./static")

  app.router.LoadHTMLGlob("views/**")

  app.router.GET("/query/count/from/:start/until/:end", app.queryCount)
  app.router.GET("/query/rowcount/from/:start/until/:end", app.queryRowCount)

  return app
}

func (app webApp) StartWebapp(appcfg appConfig) webApp {
  app.router.Run(fmt.Sprintf("%s:%d", appcfg.host, appcfg.port))

  return app
}

func (app webApp) StopWebapp() webApp {
  // TODO: implement, probably need to manage http server explicitly for that

  fmt.Printf("StopWebapp currently not implemented!")

  return app
}

func makeIsUserIdFormatIsOk() func(string) bool {
  re, err := regexp.Compile("^[[:alnum:]]*[\\w]+[[:alnum:]]$")
  if err != nil {
    panic(err)
  }

  return func (userid string) bool {
    return userid != "" && re.MatchString(userid)
  }
}

func make_authenticate(header string, devUser *string) func(*gin.Context) {
  isUserIdFormatOk := makeIsUserIdFormatIsOk()

  return func (c *gin.Context) {
    headers := c.Request.Header

    userid := headers.Get(header)
    if userid == "" && devUser != nil {
      userid = *devUser
    }

    fmt.Printf("Userid: %s - Headers: %s\n", userid, headers)

    if isUserIdFormatOk(userid) {
      c.Set("userid", userid)
    } else {
      c.AbortWithStatus(http.StatusUnauthorized)
    }
  }
}

func getUserId(c *gin.Context) (string, bool) {
  if userid, exists := c.Get("userid"); exists {
    return userid.(string), true
  } else {
    return "", false
  }
}

func (app webApp) queryCount(c *gin.Context) {
  userid, exists := getUserId(c)
  if !exists {
    panic("userid missing in handler context")
  }

  var start, end uint
  if s, err := strconv.Atoi(c.Param("start")); err == nil {
    start = uint(s)
  } else {
    panic(err)
  }
  if e, err := strconv.Atoi(c.Param("end")); err == nil {
    end = uint(e)
  } else {
    panic(err)
  }

  // parseXml(xmlData)

  variables := []map[string]any{
    {
      "key": "wantedBegin",
      "type": "integer",
      // "value": 1715689600,
      "value": start,
    },
    {
      "key": "wantedEnd",
      "type": "integer",
      // "value": 1742974400,
      "value": end,
    },
  }

  results, err := query[countResultItem](app.db, userid, "query_count", variables)
  if err != nil {
    panic(err)
  }

  // fmt.Printf("==================================== %s\n", results)

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
  c.HTML(http.StatusOK, "queryCountResult.tmpl", gin.H{
    "title": "thetitle3",
    "start": start,
    "end": end,
    "data": results,
  })
}

func (app webApp) queryRowCount(c *gin.Context) {
  userid, exists := getUserId(c)
  if !exists {
    panic("userid missing in handler context")
  }

  var start, end uint
  if s, err := strconv.Atoi(c.Param("start")); err == nil {
    start = uint(s)
  } else {
    panic(err)
  }
  if e, err := strconv.Atoi(c.Param("end")); err == nil {
    end = uint(e)
  } else {
    panic(err)
  }

  // parseXml(xmlData)

  variables := []map[string]any{
    {
      "key": "wantedBegin",
      "type": "integer",
      // "value": 1715689600,
      "value": start,
    },
    {
      "key": "wantedEnd",
      "type": "integer",
      // "value": 1742974400,
      "value": end,
    },
  }

  results, err := query[rowCountResultItem](app.db, userid, "query_row_count", variables)
  if err != nil {
    panic(err)
  }

  // fmt.Printf("==================================== %s\n", results)

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
  c.HTML(http.StatusOK, "queryRowCountResult.tmpl", gin.H{
    "title": "thetitle3",
    "start": start,
    "end": end,
    "data": results,
  })
}

package main

import (
	"fmt"
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

  //app.router.Use(make_authenticate(args.authuserHeader, args.devAuthuser))
  app.router.Use(make_authenticate(
		appcfg.authUserHeader,
		appcfg.devAuthuser,
		))

  app.router.Static("/static", "./static")

  app.router.LoadHTMLGlob("views/**")

  // router.GET("/albums/:id", getAlbumById)
  // router.GET("/albums", getAlbums)
  // router.POST("/albums", postAlbums)
  app.router.GET("/query/count/:start/:end", app.queryCount)
  app.router.GET("/query/rowcount/:start/:end", app.queryRowCount)

  // router.Run("localhost:8081")

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

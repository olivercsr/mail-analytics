package main

import (
  "os"
  // "io"
  "fmt"
  "strings"
  "regexp"
  "net/http"
  "encoding/xml"
  "github.com/gin-gonic/gin"
  "github.com/antchfx/xmlquery"
)

type album struct {
  XMLName xml.Name `xml:"album"`
  ID string `json:"id" xml:"id"` 
  Title string `json:"title" xml:"title"`
  Artist string `json:"artist" xml:"artist"`
  Price float64 `json:"price" xml:"price"`
}

var xmlData = `
<album>
  <id>123</id>
  <title>title1</title>
  <artist>artist1</artist>
  <price>39.99</price>
</album>
`

var albums = []album{
  {ID: "1", Title: "Blue Terrain", Artist: "John Coltrane", Price: 56.99},
  {ID: "2", Title: "Jeru", Artist: "Gerry Mulligan", Price: 17.99},
  {ID: "3", Title: "Sarah Vaughan and Clifford Brown", Artist: "Sarah Vaughan", Price: 39.99},
}

func parseXml(xmlData string) {
  var album album;
  err := xml.Unmarshal([]byte(xmlData), &album)
  if err != nil {
    fmt.Fprintf(os.Stderr, "Error unmarshalling XML: %v\n", err)
    return
  }

  fmt.Println("--- Processed via Unmarshalling ---")
  fmt.Printf("ID: %s, Title: %s, Artist: %s, Price: %.2f\n",
    album.ID, album.Title, album.Artist, album.Price)

  root, err := xmlquery.Parse(strings.NewReader(xmlData))
  queryResults, err := xmlquery.QueryAll(root, "//album")
  if err != nil {
    fmt.Fprintf(os.Stderr, "Error querying album xml: %v\n", err)
  } else {
    fmt.Printf("Album Xml: ")
    for _, node := range queryResults {
      fmt.Println(node.InnerText())
    }
  }
}

func getAlbums(c *gin.Context) {
  c.IndentedJSON(http.StatusOK, albums)
}

func postAlbums(c *gin.Context) {
  var newAlbum album

  if err := c.BindJSON(&newAlbum); err != nil {
    return
  }

  albums = append(albums, newAlbum)
  c.IndentedJSON(http.StatusCreated, newAlbum)
}

func getAlbumById(c *gin.Context) {
  id := c.Param("id")

  for _, a := range albums {
    if a.ID == id {
      c.IndentedJSON(http.StatusOK, a)
      return
    }
  }
  c.IndentedJSON(http.StatusNotFound, gin.H{"message": "album not found"})
}

func queryCount(c *gin.Context) {
  start, end := c.Param("start"), c.Param("end")

  parseXml(xmlData)

  c.IndentedJSON(http.StatusNotFound, gin.H{"start": start, "end": end})
}

func make_authenticate(header string) func(*gin.Context) {
  re, err := regexp.Compile("^[[:alnum:]]*[\\w]+[[:alnum:]]$")
  if err != nil {
    panic(err)
  }

  return func (c *gin.Context) {
    headers := c.Request.Header
    fmt.Printf("Headers: %s %s\n", headers, headers.Get(header))

    userid := headers.Get("remote-user")
    if userid != "" && re.MatchString(userid) {
      c.Set("userid", userid)
    } else {
      c.AbortWithStatus(401)
    }
  }
}

func main() {
  //f := func (x int) int {
  //  return x + 1
  //}
  //fmt.Printf("x: %d\n", f(11))

  router := gin.Default()

  router.Use(make_authenticate("remote-user"))

  router.GET("/albums/:id", getAlbumById)
  router.GET("/albums", getAlbums)
  router.POST("/albums", postAlbums)
  router.GET("/query/count/:start/:end", queryCount)

  router.Run("localhost:8081")
}

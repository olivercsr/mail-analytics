package main

import (
  "bytes"
  "os"
  "io"
  // "flag"
  "fmt"
  "strings"
  // "time"
  // "regexp"
  "net/http"
  "text/template"
  // "encoding/xml"
  // "github.com/gin-gonic/gin"
  "github.com/antchfx/xmlquery"
  // "github.com/cbroglie/mustache"
)

type cliArgs struct {
  authuserHeader string
  devAuthuser string
}

// type album struct {
//   XMLName xml.Name `xml:"album"`
//   ID string `json:"id" xml:"id"` 
//   Title string `json:"title" xml:"title"`
//   Artist string `json:"artist" xml:"artist"`
//   Price float64 `json:"price" xml:"price"`
// }

type existDb struct {
  uri string
}

// var xmlData = `
// <album>
//   <id>123</id>
//   <title>title1</title>
//   <artist>artist1</artist>
//   <price>39.99</price>
// </album>
// `

// var albums = []album{
//   {ID: "1", Title: "Blue Terrain", Artist: "John Coltrane", Price: 56.99},
//   {ID: "2", Title: "Jeru", Artist: "Gerry Mulligan", Price: 17.99},
//   {ID: "3", Title: "Sarah Vaughan and Clifford Brown", Artist: "Sarah Vaughan", Price: 39.99},
// }

/*
func parseCliArgs() cliArgs {
  authheader := flag.String("authuser-header", "remote-user", "HTTP header that contains the authenticated users' name.")
  authuser := flag.String("dev-authuser", "", "Set authuser to this value (useful for dev/debugging).")

  flag.Parse()

  args := cliArgs{
    authuserHeader: *authheader,
    devAuthuser: *authuser,
  }

  fmt.Println("args", args)

  return args
}
*/

/*
type viewRenderer struct {
  partialsProvider mustache.PartialProvider
}

func newViewRenderer(path string) viewRenderer {
  return viewRenderer{
    partialsProvider: &mustache.FileProvider{
      Paths: []string{path},
      Extensions: []string{".html"},
    },
  }
}

func (renderer viewRenderer) render(viewName string, data map[string]string) string {
  rootView := fmt.Sprintf("{{> %s }}", viewName)

  rendered, err := mustache.RenderPartials(rootView, renderer.partialsProvider, data)
  if err != nil {
    panic(err)
  }

  return rendered
}
*/

func renderXquery() string {
  tmpl := template.Must(template.ParseGlob("queries/**"))

  data := map[string][]map[string]any{
    "variables": {
      {
        "key": "wantedBegin",
        "type": "integer",
        "value": 1715689600,
      },
      {
        "key": "wantedEnd",
        "type": "integer",
        "value": 1742974400,
      },
    },
  }

  buf := new(bytes.Buffer)
  err := tmpl.ExecuteTemplate(buf, "query_count.xquery", data)
  if err != nil {
    panic(err)
  }

  return buf.String()
}

func parseXml(xmlData string) {
  // time.Sleep(5 * time.Second)

  /*
  var album album
  err := xml.Unmarshal([]byte(xmlData), &album)
  if err != nil {
    fmt.Fprintf(os.Stderr, "Error unmarshalling XML: %v\n", err)
    return
  }

  fmt.Println("--- Processed via Unmarshalling ---")
  fmt.Printf("ID: %s, Title: %s, Artist: %s, Price: %.2f\n",
    album.ID, album.Title, album.Artist, album.Price)
  */

  root, err := xmlquery.Parse(strings.NewReader(xmlData))
  queryResults, err := xmlquery.QueryAll(root, "//rowcount")
  if err != nil {
    fmt.Fprintf(os.Stderr, "Error querying album xml: %v\n", err)
  } else {
    fmt.Printf("Xpath result: ")
    for _, node := range queryResults {
      fmt.Println(node.InnerText())
    }
  }
}

func (db existDb) queryDb(query string) (string, error) {
  buf := strings.NewReader(query)
  resp, err := http.Post(db.uri, "text/xml", buf)
  if err != nil {
    panic(err)
  }
  defer resp.Body.Close()

  body, err := io.ReadAll(resp.Body)
  if err != nil {
    panic(err)
  }

  return string(body), nil
}

// func getAlbums(c *gin.Context) {
//   c.IndentedJSON(http.StatusOK, albums)
// }
//
// func postAlbums(c *gin.Context) {
//   var newAlbum album
//
//   if err := c.BindJSON(&newAlbum); err != nil {
//     return
//   }
//
//   albums = append(albums, newAlbum)
//   c.IndentedJSON(http.StatusCreated, newAlbum)
// }
//
// func getAlbumById(c *gin.Context) {
//   id := c.Param("id")
//
//   for _, a := range albums {
//     if a.ID == id {
//       c.IndentedJSON(http.StatusOK, a)
//       return
//     }
//   }
//   c.IndentedJSON(http.StatusNotFound, gin.H{"message": "album not found"})
// }

func main() {
  //args := parseCliArgs()
  appcfg, err := ReadAppConfig()
  if err != nil {
		panic(err)
  }

  db := existDb{
    uri: appcfg.existdbUri,
  }

  webapp := NewWebapp(appcfg, db)
	webapp.StartWebapp(appcfg)
}


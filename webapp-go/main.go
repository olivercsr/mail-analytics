package main

import (
  //"os"
  "fmt"
  "net/http"
  "github.com/gin-gonic/gin"
)

type album struct {
  ID string `json:"id"`
  Title string `json:"title"`
  Artist string `json:"artist"`
  Price float64 `json:"price"`
}

var albums = []album{
  {ID: "1", Title: "Blue Terrain", Artist: "John Coltrane", Price: 56.99},
  {ID: "2", Title: "Jeru", Artist: "Gerry Mulligan", Price: 17.99},
  {ID: "3", Title: "Sarah Vaughan and Clifford Brown", Artist: "Sarah Vaughan", Price: 39.99},
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

func check_auth(c *gin.Context) {
  var a = c.Request.Header
  println(fmt.Sprintf("Headers: %s\n", a))
  c.AbortWithStatus(401)
}

func main() {
  router := gin.Default()

  router.Use(check_auth)

  router.GET("/albums/:id", getAlbumById)
  router.GET("/albums", getAlbums)
  router.POST("/albums", postAlbums)

  router.Run("localhost:8081")
}

package main

import ()

func main() {
  appcfg, err := ReadAppConfig()
  if err != nil {
		panic(err)
  }

  db := NewExistDb(appcfg)

  webapp := NewWebapp(appcfg, db)
	webapp.StartWebapp(appcfg)
}


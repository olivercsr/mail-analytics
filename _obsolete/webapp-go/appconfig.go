package main

import (
  "errors"
  "os"
  "strconv"
)

type appConfig struct {
  loglevel string
  host string
  port uint16
  existdbUri string
  authUserHeader string
  authGroupsHeader string
  devAuthuser *string
}

func (cfg appConfig) init() appConfig {
  cfg.loglevel = "info"
  cfg.host = "localhost"
  cfg.port = 8080
  cfg.existdbUri = "http://localhost:8081/exist/rest/dmarc"
  cfg.authUserHeader = "remote-user"

  return cfg
}

func ReadAppConfig() (appConfig, error) {
  cfg := appConfig{}.init()

  if val, ok := os.LookupEnv("LOGLEVEL"); ok {
    cfg.loglevel = val
  }
  if val, ok := os.LookupEnv("HOST"); ok {
    cfg.host = val
  }
  if val, ok := os.LookupEnv("PORT"); ok {
    if ival, err := strconv.Atoi(val); err == nil {
      cfg.port = uint16(ival)
    } else {
      return appConfig{}, errors.New("could not parse value of env var PORT")
    }
  }
  if val, ok := os.LookupEnv("EXISTDB_URI"); ok {
    cfg.existdbUri = val
  }
  if val, ok := os.LookupEnv("AUTHUSER_HEADER"); ok {
    cfg.authUserHeader = val
  }
  if val, ok := os.LookupEnv("AUTHGROUPS_HEADER"); ok {
    cfg.authGroupsHeader = val
  }
  if val, ok := os.LookupEnv("DEV_AUTHUSER"); ok {
    cfg.devAuthuser = &val
  }

  return cfg, nil
}


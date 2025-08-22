export const hooks = {
  mounted() {
    this.handleEvent("storeSession", (obj) => this.store(obj))
    this.handleEvent("clearSession", (obj) => this.clear(obj))
    this.handleEvent("restoreSession", (obj) => this.restore(obj))

    this.handleEvent("storeLocal", (obj) => this.store(obj))
    this.handleEvent("clearLocal", (obj) => this.clear(obj))
    this.handleEvent("restoreLocal", (obj) => this.restore(obj))
  },

  storeSession(obj) {
    sessionStorage.setItem(obj.key, obj.data)
  },

  restoreSession(obj) {
    var data = sessionStorage.getItem(obj.key)
    this.pushEvent(obj.event, data)
  },

  clearSession(obj) {
    sessionStorage.removeItem(obj.key)
  },

  storeLocal(obj) {
    localStorage.setItem(obj.key, obj.data)
  },

  restoreLocal(obj) {
    var data = localStorage.getItem(obj.key)
    this.pushEvent(obj.event, data)
  },

  clearLocal(obj) {
    localStorage.removeItem(obj.key)
  }
}


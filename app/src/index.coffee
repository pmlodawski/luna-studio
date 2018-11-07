{app, BrowserWindow} = require('electron')

# Global reference in order to prevent garbage collecting
mainWindow = null

createWindow = () ->
  mainWindow = new BrowserWindow
    width  : 800
    height : 600

  mainWindow.loadURL 'http://localhost:9000/'

  # mainWindow.webContents.openDevTools()

  mainWindow.on 'closed', () ->
    mainWindow = null

app.on 'ready', createWindow

app.on 'window-all-closed', () ->
  if (process.platform != 'darwin')
    app.quit()

app.on 'activate', () ->
  if (mainWindow == null)
    createWindow()

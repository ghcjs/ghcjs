const electron = require('electron')
const app = electron.app
const BrowserWindow = electron.BrowserWindow
const Menu = electron.Menu

const path = require('path')
const url = require('url')
const fs = require('fs')
const readline = require('readline');

// Keep a global reference of the window object, if you don't, the window will
// be closed automatically when the JavaScript object is garbage collected.
let mainWindow

var args;
var dummyIdx = process.argv.indexOf('--ghcjs-electron-dummy-arg');
if(dummyIdx >= 0) {
  args = process.argv.slice(dummyIdx+1);
} else {
  // assume packaged electron app
  args = process.argv.slice(1);
}

console.log(args);

var pageURL = url.format({
      pathname: path.join(__dirname, 'electron.html'),
      protocol: 'file:',
      slashes: true
    });
if(args.length > 0) {
  if(args[0].match(/^https?:\/\/.*$/i)) {
    pageURL = args[0];
  } else {
    // fixme try to load files from the command line?
  }
}

function createWindow () {
  // BrowserWindow.addDevToolsExtension(path.join(__dirname, 'ext', 'ghcjs'))

  mainWindow = new BrowserWindow({width: 800, height: 600})

  mainWindow.loadURL(pageURL);

  mainWindow.webContents.openDevTools()

  // move devtools to a different WebContents?
  // contents.setDevToolsWebContents(devToolsWebContents)

  mainWindow.on('closed', function () {
    mainWindow = null
  })
}

app.on('browser-window-created', function(ev, browserWindow) {
//  console.log("browser window created")
//  console.log(browserWindow)
})

/*
  the script that we execute in the devtools

  This script sets up interception of inspector protocol messages for Haskell
  debugging.
 */
var injectJs = fs.readFileSync( path.join( __dirname
                                         , 'electron-devtools-inject.js')
                              , {encoding:'utf8'});

var injectJsRenderer = fs.readFileSync( path.join ( __dirname
                                                  , 'electron-renderer-inject.js')
                                      , {encoding:'utf8'});

// simple prompt that evaluates entered JS expressions in the inspector
// context. useful for debugging the devtools. results are printed to the
// devtools console
var devREPLStarted = false;
function startDevREPL(contents) {
  if(devREPLStarted) return;
  devREPLStarted = true;
  var rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout
  });
  rl.setPrompt('> ');
  rl.on('line', function(line) {
    contents.executeJavaScript("replEvalJS("+JSON.stringify(line)+");");
    rl.prompt();
  });
  rl.on('close', function() {
    // console.log("repl closed");
    process.exit(0);
  });
  // small delay to show prompt after initial warnings and startup messages
  setTimeout(function() {
    rl.prompt();
  }, 1000);
}

app.on('web-contents-created', function(ev, webContents) {
  //console.log("web contents created")
  //console.log(webContents)
  //console.log(webContents.getURL())
  // webContents.on('will-attach-webview', function(event, webPreferences, params) {
    //console.log("will-attach-webview");
    //console.log(webPreferences);
  // });

  if(webContents.getURL().startsWith('chrome-devtools:')) {
    // console.log("devtools opened!");
    // webContents.executeJavaScript("document.write(JSON.stringify(window.chrome));")
    // webContents.executeJavaScript("for(var i in window) { document.write(i+'<br>'); }")
    //webContents.on('console-message', function(lvl, msg, line, sourceId) {
//      console.log(msg);
    //})
    webContents.executeJavaScript(injectJs);
    startDevREPL(webContents); // there will only be a REPL for the first devtools opened
    // debug@
    // setTimeout(function() {
    //   webContents.executeJavaScript("_replEvalJS(\"test0()\");");
    // }, 800);

    // webContents.executeJavaScript("alert('This is a rather simple example...');")

  } else {
    webContents.executeJavaScript(injectJsRenderer);
  }
})

// This method will be called when Electron has finished
// initialization and is ready to create browser windows.
// Some APIs can only be used after this event occurs.
app.on('ready', createWindow)

// Quit when all windows are closed.
app.on('window-all-closed', function () {
  // On OS X it is common for applications and their menu bar
  // to stay active until the user quits explicitly with Cmd + Q
  if (process.platform !== 'darwin') {
    app.quit()
  }
})

app.on('activate', function () {
  // On OS X it's common to re-create a window in the app when the
  // dock icon is clicked and there are no other windows open.
  if (mainWindow === null) {
    createWindow()
  }
})

let template = [{
  label: 'Edit',
  submenu: [{
    label: 'Undo',
    accelerator: 'CmdOrCtrl+Z',
    role: 'undo'
  }, {
    label: 'Redo',
    accelerator: 'Shift+CmdOrCtrl+Z',
    role: 'redo'
  }, {
    type: 'separator'
  }, {
    label: 'Cut',
    accelerator: 'CmdOrCtrl+X',
    role: 'cut'
  }, {
    label: 'Copy',
    accelerator: 'CmdOrCtrl+C',
    role: 'copy'
  }, {
    label: 'Paste',
    accelerator: 'CmdOrCtrl+V',
    role: 'paste'
  }, {
    label: 'Select All',
    accelerator: 'CmdOrCtrl+A',
    role: 'selectall'
  }]
}, {
  label: 'View',
  submenu: [{
    label: 'Reload',
    accelerator: 'CmdOrCtrl+R',
    click: function (item, focusedWindow) {
      if (focusedWindow) {
        // on reload, start fresh and close any old
        // open secondary windows
        if (focusedWindow.id === 1) {
          BrowserWindow.getAllWindows().forEach(function (win) {
            if (win.id > 1) {
              win.close()
            }
          })
        }
        focusedWindow.reload()
      }
    }
  }, {
    label: 'Toggle Full Screen',
    accelerator: (function () {
      if (process.platform === 'darwin') {
        return 'Ctrl+Command+F'
      } else {
        return 'F11'
      }
    })(),
    click: function (item, focusedWindow) {
      if (focusedWindow) {
        focusedWindow.setFullScreen(!focusedWindow.isFullScreen())
      }
    }
  }, {
    label: 'Toggle Developer Tools',
    accelerator: (function () {
      if (process.platform === 'darwin') {
        return 'Alt+Command+I'
      } else {
        return 'Ctrl+Shift+I'
      }
    })(),
    click: function (item, focusedWindow) {
      if (focusedWindow) {
        focusedWindow.toggleDevTools()
      }
    }
  }, {
    type: 'separator'
  }, {
    label: 'App Menu Demo',
    click: function (item, focusedWindow) {
      if (focusedWindow) {
        const options = {
          type: 'info',
          title: 'Application Menu Demo',
          buttons: ['Ok'],
          message: 'This demo is for the Menu section, showing how to create a clickable menu item in the application menu.'
        }
        electron.dialog.showMessageBox(focusedWindow, options, function () {})
      }
    }
  }]
}, {
  label: 'Window',
  role: 'window',
  submenu: [{
    label: 'Minimize',
    accelerator: 'CmdOrCtrl+M',
    role: 'minimize'
  }, {
    label: 'Close',
    accelerator: 'CmdOrCtrl+W',
    role: 'close'
  }, {
    type: 'separator'
  }, {
    label: 'Reopen Window',
    accelerator: 'CmdOrCtrl+Shift+T',
    enabled: false,
    key: 'reopenMenuItem',
    click: function () {
      app.emit('activate')
    }
  }]
}, {
  label: 'Help',
  role: 'help',
  submenu: [{
    label: 'Learn More',
    click: function () {
      electron.shell.openExternal('http://github.com/ghcjs/ghcjs')
    }
  }]
}]

function findReopenMenuItem () {
  const menu = Menu.getApplicationMenu()
  if (!menu) return

  let reopenMenuItem
  menu.items.forEach(function (item) {
    if (item.submenu) {
      item.submenu.items.forEach(function (item) {
        if (item.key === 'reopenMenuItem') {
          reopenMenuItem = item
        }
      })
    }
  })
  return reopenMenuItem
}

if (process.platform === 'darwin') {
  const name = electron.app.getName()
  template.unshift({
    label: name,
    submenu: [{
      label: `About ${name}`,
      role: 'about'
    }, {
      type: 'separator'
    }, {
      label: 'Services',
      role: 'services',
      submenu: []
    }, {
      type: 'separator'
    }, {
      label: `Hide ${name}`,
      accelerator: 'Command+H',
      role: 'hide'
    }, {
      label: 'Hide Others',
      accelerator: 'Command+Alt+H',
      role: 'hideothers'
    }, {
      label: 'Show All',
      role: 'unhide'
    }, {
      type: 'separator'
    }, {
      label: 'Quit',
      accelerator: 'Command+Q',
      click: function () {
        app.quit()
      }
    }]
  })

  // Window menu.
  template[3].submenu.push({
    type: 'separator'
  }, {
    label: 'Bring All to Front',
    role: 'front'
  })

}

if (process.platform === 'win32') {
  const helpMenu = template[template.length - 1].submenu
}

app.on('ready', function () {
  const menu = Menu.buildFromTemplate(template)
  Menu.setApplicationMenu(menu)
})

app.on('browser-window-created', function () {
  let reopenMenuItem = findReopenMenuItem()
  if (reopenMenuItem) reopenMenuItem.enabled = false
})

app.on('window-all-closed', function () {
  let reopenMenuItem = findReopenMenuItem()
  if (reopenMenuItem) reopenMenuItem.enabled = true
})

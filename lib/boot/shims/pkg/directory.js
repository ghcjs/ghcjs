#include "HsBaseConfig.h"

#ifdef GHCJS_TRACE_DIRECTORY
function h$logDirectory() { h$log.apply(h$log,arguments); }
#define TRACE_DIRECTORY(args...) h$logDirectory(args)
#else
#define TRACE_DIRECTORY(args...)
#endif

// get/set permissions for file
// set errno and return -1 on error
// masks: 1 - read
//        2 - write
//        4 - exe
//        8 - search
function h$directory_getPermissions(file, c) {
    TRACE_DIRECTORY("getPermissions: " + file);
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        h$fs.stat(file, function(err, fs) {
            if(err) {
                h$handleErrnoC(err, -1, 0, c);
            } else {
                var m = fs.mode;
                var r = (m&4) || (m&32) || (m&256);
                var w = (m&2) || (m&16) || (m&128);
                var x = (m&1) || (m&8)  || (m&64);
                var exe    = x; // fixme?
                var search = x; // fixme?
                if(process.platform == 'win32') exe = true;
                c((r?1:0)|(w?2:0)|(exe?4:0)|(search?8:0));
            }
        });
    } else
#endif
        h$unsupported(-1, c);
}

function h$directory_setPermissions(file, perms, c) {
    TRACE_DIRECTORY("setPermissions: " + file + " " + perms);
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        h$fs.stat(file, function(err, fs) {
            if(err) {
                h$handleErrnoC(err, -1, 0, c);
            } else {
                var r = perms & 1;
                var w = perms & 2;
                var x = perms & 4;
                var search = perms & 8;
                var m  = fs.mode;
                m = r ? (m | 292) : (m & ~292);
                m = w ? (m | 146) : (m & ~146);
                m = (x || search) ? (m | 73) : (m & ~73);
                h$fs.chmod(file, function(err) {
                    h$handleErrnoC(err, -1, 0, c);
                });
            }
        });
    } else
#endif
        h$unsupported(-1, c);
}

function h$directory_copyPermissions(file1, file2, c) {
    TRACE_DIRECTORY("copyPermissions: " + file1 + " " + file2);
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        h$fs.stat(file1, function(err1, fs) {
            if(err1) {
                h$handleErrnoC(err1, -1, 0, c);
            } else {
                h$fs.chmod(file2, fs.mode, function(err2) {
                    h$handleErrnoC(err2, -1, 0, c);
                });
            }
        });
    } else
#endif
        h$unsupported(-1, c);
}


function h$directory_createDirectory(dir, c) {
    TRACE_DIRECTORY("createDirectory: " + dir);
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        h$fs.mkdir(dir, function(err) {
            h$handleErrnoC(err,-1,0,c);
        });
    } else
#endif
        h$unsupported(-1, c);
}

function h$directory_removeDirectory(dir, c) {
    TRACE_DIRECTORY("removeDirectory: " + dir);
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        h$fs.rmdir(dir, function(err) {
            h$handleErrnoC(err,-1,0,c);
        });
    } else
#endif
        h$unsupported(-1, c);
}

function h$directory_removeFile(file, c) {
    TRACE_DIRECTORY("removeFile: " + file);
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        h$fs.unlink(file, function(err) {
            h$handleErrnoC(err,-1,0,c);
        });
    } else
#endif
        h$unsupported(-1, c);
}

function h$directory_renameDirectory(dir1, dir2, c) {
    TRACE_DIRECTORY("renameDirectory: " + dir1 + " " + dir2);
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        h$fs.rename(dir1, dir2, function(err) {
            h$handleErrnoC(err,-1,0,c);
        });
    } else
#endif
        h$unsupported(-1, c);
}

function h$directory_renameFile(file1, file2, c) {
    TRACE_DIRECTORY("renameFile: " + file1 + " " + file2);
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        h$fs.rename(file1, file2, function(err) {
            h$handleErrnoC(err,-1,0,c);
        });
    } else
#endif
        h$unsupported(-1, c);
}

function h$directory_canonicalizePath(path) {
    TRACE_DIRECTORY("canonicalizePath: " + path);
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        return h$path.normalize(path);
    } else
#endif
        return path;
}

function h$directory_findExecutables(name, c) {
    TRACE_DIRECTORY("findExecutables: " + name);
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        var result = [];
        var pathSep = process.platform === 'win32'?';':':';
        var parts   = process.env['PATH'].split(pathSep);
        var exts    = []; // process.platform === 'win32'?process.env['PATHEXT'].split(pathSep):[];
        exts.push(null);
        files = [];
        result = [];
        for(var i=0;i<parts.length;i++) {
            for(var j=0;j<exts.length;j++) {
                files.push(parts[i] + h$path.sep + name + (exts[j]?(exts[j]):""));
            }
        }
        var tryFile = function(n) {
            if(n >= files.length) {
                c(result);
            } else {
                TRACE_DIRECTORY("trying: " + files[n]);
                h$fs.stat(files[n], function(err, fs) {
                    if(!err && ((fs.mode & 73) || process.platform === 'win32')) result.push(files[n]);
                    tryFile(n+1);
                });
            }
        }
        tryFile(0);
    } else
#endif
        c([]);
}

function h$directory_getDirectoryContents(dir,c) {
    TRACE_DIRECTORY("getDirectoryContents: " + dir);
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        h$fs.readdir(dir, function(err, d) {
            h$handleErrnoC(err, null, d, c);
        });
    } else
#endif
        h$unsupported(null, c);
}

function h$directory_getCurrentDirectory() {
    TRACE_DIRECTORY("getCurrentDirectory");
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        return h$handleErrno(null, function() {
            return process.cwd();
        });
    } else
#endif
        return "/";
}

function h$directory_setCurrentDirectory(dir) {
    TRACE_DIRECTORY("setCurrentDirectory: " + dir);
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        return h$handleErrnoS(-1, 0, function() {
            return process.chdir(dir);
        });
    } else
#endif
        return h$unsupported(-1);
}

function h$directory_getHomeDirectory(dir) {
    TRACE_DIRECTORY("getHomeDirectory: " + dir);
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        return process.env['HOME'] ||
            process.env['HOMEPATH'] ||
            process.env['USERPROFILE'];
    } else
#endif
        return "/"
}

function h$directory_getAppUserDataDirectory(appName) {
    TRACE_DIRECTORY("getAppUserDataDirectory: " + appName);
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        if(process.env['APPDATA'])
            return process.env['APPDATA'] + h$path.sep + appName;
        if(process.env['HOME'])
            return process.env['HOME'] + h$path.sep + "." + appName;
        TRACE_DIRECTORY("getAppUserDataDirectory fallback");
        return "/";
    } else
#endif
        return "/";
}

function h$directory_getUserDocumentsDirectory(appName) {
    TRACE_DIRECTORY("getUserDocumentsDirectory: " + appName);
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        if(process.env['HOME'])
            return process.env['HOME'];
        // fixme handle Windows
        TRACE_DIRECTORY("getUserDocumentsDirectory fallback");
        return "/";
    } else
#endif
        return "/";
}

function h$directory_getTemporaryDirectory() {
    TRACE_DIRECTORY("getTemporaryDirectory");
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        return h$handleErrno(null, function() {
            return h$os.tmpdir();
        });
    } else
#endif
        return "/";
}

function h$directory_exeExtension() {
    TRACE_DIRECTORY("exeExtension");
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        return (h$os.platform() === 'windows') ? 'exe' : '';
    } else
#endif
        return '';
}

function h$directory_getFileStatus(file, c) {
    TRACE_DIRECTORY("getFileStatus: " + file);
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        h$fs.stat(file, function(err, s) {
            h$handleErrnoC(err, null, s, c);
        });
    } else
#endif
        h$unsupported(null, c);
}

function h$directory_getFileOrSymlinkStatus(file, c) {
    TRACE_DIRECTORY("getFileOrSymlinkStatus: " + file);
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        h$fs.lstat(file, function(err, s) {
            h$handleErrnoC(err, null, s, c);
        });
    } else
#endif
        h$unsupported(null, c);
}

function h$directory_getFileStatusAccessTime(fs) {
  TRACE_DIRECTORY("getFileStatusAccessTime: " + fs.atime.getTime());
  return fs.atime.getTime();
}

function h$directory_getFileStatusModificationTime(fs) {
  TRACE_DIRECTORY("getFileStatusModificationTime: " + fs.mtime.getTime());
  return fs.mtime.getTime();
}

function h$directory_getFileStatusIsDirectory(fs) {
  TRACE_DIRECTORY("getFileStatusIsDirectory: " + fs + " " + fs.isDirectory());
  return fs.isDirectory();
}

function h$directory_getFileStatusIsSymbolicLink(fs) {
  TRACE_DIRECTORY("getFileStatusIsSymbolicLink: " + fs + " " + fs.isSymbolicLink());
  return fs.isSymbolicLink();
}

// fixme this doesn't really belong here
function h$chmod(path_d, path_o, m) {
#ifndef GHCJS_BROWSER
    if(h$isNode) {
        var path = h$decodeUtf8z(path_d, path_o);
        TRACE_DIRECTORY("chmod: " + path + " mode: " + m);
        h$fs.chmodSync(path, m);
        return 0;
    } else
#endif
        return h$unsupported(-1);
}



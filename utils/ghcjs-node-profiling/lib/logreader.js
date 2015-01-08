var CsvParser = require('./csvparser');
// Copyright 2011 the V8 project authors. All rights reserved.
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
//     * Redistributions of source code must retain the above copyright
//       notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above
//       copyright notice, this list of conditions and the following
//       disclaimer in the documentation and/or other materials provided
//       with the distribution.
//     * Neither the name of Google Inc. nor the names of its
//       contributors may be used to endorse or promote products derived
//       from this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

/**
 * @fileoverview Log Reader is used to process log file produced by V8.
 */


/**
 * Base class for processing log files.
 *
 * @param {Array.<Object>} dispatchTable A table used for parsing and processing
 *     log records.
 * @constructor
 */
function LogReader(dispatchTable) {
  /**
   * @type {Array.<Object>}
   */
  this.dispatchTable_ = dispatchTable;

  /**
   * Current line.
   * @type {number}
   */
  this.lineNum_ = 0;

  /**
   * CSV lines parser.
   * @type {CsvParser}
   */
  this.csvParser_ = new CsvParser();
};


/**
 * Used for printing error messages.
 *
 * @param {string} str Error message.
 */
LogReader.prototype.printError = function(str) {
  // Do nothing.
};


/**
 * Processes a portion of V8 profiler event log.
 *
 * @param {string} chunk A portion of log.
 */
LogReader.prototype.processLogChunk = function(chunk) {
  this.processLog_(chunk.split('\n'));
};


/**
 * Processes a line of V8 profiler event log.
 *
 * @param {string} line A line of log.
 */
LogReader.prototype.processLogLine = function(line) {
  this.processLog_([line]);
};


/**
 * Processes stack record.
 *
 * @param {number} pc Program counter.
 * @param {number} func JS Function.
 * @param {Array.<string>} stack String representation of a stack.
 * @return {Array.<number>} Processed stack.
 */
LogReader.prototype.processStack = function(pc, func, stack) {
  var fullStack = func ? [pc, func] : [pc];
  var prevFrame = pc;
  for (var i = 0, n = stack.length; i < n; ++i) {
    var frame = stack[i];
    var firstChar = frame.charAt(0);
    if (firstChar == '+' || firstChar == '-') {
      // An offset from the previous frame.
      prevFrame += parseInt(frame, 16);
      fullStack.push(prevFrame);
    // Filter out possible 'overflow' string.
    } else if (firstChar != 'o') {
      fullStack.push(parseInt(frame, 16));
    }
  }
  return fullStack;
};


/**
 * Returns whether a particular dispatch must be skipped.
 *
 * @param {!Object} dispatch Dispatch record.
 * @return {boolean} True if dispatch must be skipped.
 */
LogReader.prototype.skipDispatch = function(dispatch) {
  return false;
};


/**
 * Does a dispatch of a log record.
 *
 * @param {Array.<string>} fields Log record.
 * @private
 */
LogReader.prototype.dispatchLogRow_ = function(fields) {
  // Obtain the dispatch.
  var command = fields[0];
  if (!(command in this.dispatchTable_)) return;

  var dispatch = this.dispatchTable_[command];

  if (dispatch === null || this.skipDispatch(dispatch)) {
    return;
  }

  // special case so we can parse v8.log from both 0.10 and 0.11
  // after first 'code-creation' with less than 5 fields we assume that it's from
  // v8 3.14.5.9 and 'kind' field is missing from second position
  // ( present in v8 3.26.33 profile, node v0.11+ )
  if (command == 'code-creation' && fields.length <= 5) {
     if (typeof dispatch.codeCreationKind == "undefined") {
       dispatch.codeCreationKind = false;
       dispatch.parsers = [null, 'void', parseInt, parseInt, null, 'var-args'];
     }
  }

  // Parse fields.
  var parsedFields = [];
  var fieldIndex  = 1;
  var parserIndex = 0;
  var parser;

  for ( ;parserIndex < dispatch.parsers.length; ++fieldIndex, ++parserIndex) {
    parser = dispatch.parsers[parserIndex];
    if (parser === null) {
      parsedFields.push(fields[fieldIndex]);
    } else if (typeof parser == 'function') {
      parsedFields.push(parser(fields[fieldIndex]));
    } else if (parser === 'void') {
      // add undefined to parameters, but don't consume field
      parsedFields.push(void(0));
      --fieldIndex;
    } else {
      // var-args
      parsedFields.push(fields.slice(fieldIndex));
      break;
    }
  }

  // Run the processor.
  dispatch.processor.apply(this, parsedFields);
};


/**
 * Processes log lines.
 *
 * @param {Array.<string>} lines Log lines.
 * @private
 */
LogReader.prototype.processLog_ = function(lines) {
  for (var i = 0, n = lines.length; i < n; ++i, ++this.lineNum_) {
    var line = lines[i];
    if (!line) {
      continue;
    }
    try {
      var fields = this.csvParser_.parseLine(line);
      this.dispatchLogRow_(fields);
    } catch (e) {
      this.printError('line ' + (this.lineNum_ + 1) + ': ' + (e.message || e));
    }
  }
};
module.exports.LogReader = LogReader;

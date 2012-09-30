// Graphics.UI.Gtk.WebKit.DOM.Xpath
webkit_dom_xpath_result_get_type = (function()
                                    {
                                      return XPathResult;
                                    });
var webkit_dom_xpath_result_iterate_next;
webkit_dom_xpath_result_iterate_next = (function(self)
                                        {
                                          return self["iterateNext"]();
                                        });
var webkit_dom_xpath_result_snapshot_item;
webkit_dom_xpath_result_snapshot_item = (function(self, index)
                                         {
                                           return self["snapshotItem"]($hs_intToNumber(index));
                                         });
var webkit_dom_xpath_result_get_result_type;
webkit_dom_xpath_result_get_result_type = (function(self)
                                           {
                                             return $hs_int(self["resultType"]);
                                           });
var webkit_dom_xpath_result_get_number_value;
webkit_dom_xpath_result_get_number_value = (function(self)
                                            {
                                              return self["numberValue"];
                                            });
var webkit_dom_xpath_result_get_string_value;
webkit_dom_xpath_result_get_string_value = (function(self)
                                            {
                                              return $hs_toUtf8(self["stringValue"]);
                                            });
var webkit_dom_xpath_result_get_boolean_value;
webkit_dom_xpath_result_get_boolean_value = (function(self)
                                             {
                                               return $hs_int((self["booleanValue"] ? 1 : 0));
                                             });
var webkit_dom_xpath_result_get_single_node_value;
webkit_dom_xpath_result_get_single_node_value = (function(self)
                                                 {
                                                   return self["singleNodeValue"];
                                                 });
var webkit_dom_xpath_result_get_invalid_iterator_state;
webkit_dom_xpath_result_get_invalid_iterator_state = (function(self)
                                                      {
                                                        return $hs_int((self["invalidIteratorState"] ? 1 : 0));
                                                      });
var webkit_dom_xpath_result_get_snapshot_length;
webkit_dom_xpath_result_get_snapshot_length = (function(self)
                                               {
                                                 return $hs_int(self["snapshotLength"]);
                                               });
// Graphics.UI.Gtk.WebKit.DOM.Xpath
webkit_dom_xpath_ns_resolver_get_type = (function()
                                         {
                                           return XPathNSResolver;
                                         });
var webkit_dom_xpath_ns_resolver_lookup_namespace_uri;
webkit_dom_xpath_ns_resolver_lookup_namespace_uri = (function(self,
                                                              prefix)
                                                     {
                                                       return $hs_toUtf8(self["lookupNamespaceURI"]($hs_fromUtf8(prefix)));
                                                     });
// Graphics.UI.Gtk.WebKit.DOM.Xpath
webkit_dom_xpath_expression_get_type = (function()
                                        {
                                          return XPathExpression;
                                        });
var webkit_dom_xpath_expression_evaluate;
webkit_dom_xpath_expression_evaluate = (function(self, contextNode,
                                                 type, inResult)
                                        {
                                          return self["evaluate"](contextNode,
                                                                  $hs_intToNumber(type), inResult);
                                        });
// Graphics.UI.Gtk.WebKit.DOM.Xml
webkit_dom_xml_http_request_get_type = (function()
                                        {
                                          return XMLHttpRequest;
                                        });
var webkit_dom_xml_http_request_set_request_header;
webkit_dom_xml_http_request_set_request_header = (function(self,
                                                           header, value)
                                                  {
                                                    return self["setRequestHeader"]($hs_fromUtf8(header),
                                                                                    $hs_fromUtf8(value));
                                                  });
var webkit_dom_xml_http_request_abort;
webkit_dom_xml_http_request_abort = (function(self)
                                     {
                                       return self["abort"]();
                                     });
var webkit_dom_xml_http_request_get_all_response_headers;
webkit_dom_xml_http_request_get_all_response_headers = (function(self)
                                                        {
                                                          return $hs_toUtf8(self["getAllResponseHeaders"]());
                                                        });
var webkit_dom_xml_http_request_get_response_header;
webkit_dom_xml_http_request_get_response_header = (function(self,
                                                            header)
                                                   {
                                                     return $hs_toUtf8(self["getResponseHeader"]($hs_fromUtf8(header)));
                                                   });
var webkit_dom_xml_http_request_override_mime_type;
webkit_dom_xml_http_request_override_mime_type = (function(self,
                                                           override)
                                                  {
                                                    return self["overrideMimeType"]($hs_fromUtf8(override));
                                                  });
var webkit_dom_xml_http_request_dispatch_event;
webkit_dom_xml_http_request_dispatch_event = (function(self, evt)
                                              {
                                                return $hs_int((self["dispatchEvent"](evt) ? 1 : 0));
                                              });
var webkit_dom_xml_http_request_set_onabort;
webkit_dom_xml_http_request_set_onabort = (function(self, val)
                                           {
                                             self["onabort"] = val;
                                           });
var webkit_dom_xml_http_request_get_onabort;
webkit_dom_xml_http_request_get_onabort = (function(self)
                                           {
                                             return self["onabort"];
                                           });
var webkit_dom_xml_http_request_set_onerror;
webkit_dom_xml_http_request_set_onerror = (function(self, val)
                                           {
                                             self["onerror"] = val;
                                           });
var webkit_dom_xml_http_request_get_onerror;
webkit_dom_xml_http_request_get_onerror = (function(self)
                                           {
                                             return self["onerror"];
                                           });
var webkit_dom_xml_http_request_set_onload;
webkit_dom_xml_http_request_set_onload = (function(self, val)
                                          {
                                            self["onload"] = val;
                                          });
var webkit_dom_xml_http_request_get_onload;
webkit_dom_xml_http_request_get_onload = (function(self)
                                          {
                                            return self["onload"];
                                          });
var webkit_dom_xml_http_request_set_onloadend;
webkit_dom_xml_http_request_set_onloadend = (function(self, val)
                                             {
                                               self["onloadend"] = val;
                                             });
var webkit_dom_xml_http_request_get_onloadend;
webkit_dom_xml_http_request_get_onloadend = (function(self)
                                             {
                                               return self["onloadend"];
                                             });
var webkit_dom_xml_http_request_set_onloadstart;
webkit_dom_xml_http_request_set_onloadstart = (function(self, val)
                                               {
                                                 self["onloadstart"] = val;
                                               });
var webkit_dom_xml_http_request_get_onloadstart;
webkit_dom_xml_http_request_get_onloadstart = (function(self)
                                               {
                                                 return self["onloadstart"];
                                               });
var webkit_dom_xml_http_request_set_onprogress;
webkit_dom_xml_http_request_set_onprogress = (function(self, val)
                                              {
                                                self["onprogress"] = val;
                                              });
var webkit_dom_xml_http_request_get_onprogress;
webkit_dom_xml_http_request_get_onprogress = (function(self)
                                              {
                                                return self["onprogress"];
                                              });
var webkit_dom_xml_http_request_set_onreadystatechange;
webkit_dom_xml_http_request_set_onreadystatechange = (function(self,
                                                               val)
                                                      {
                                                        self["onreadystatechange"] = val;
                                                      });
var webkit_dom_xml_http_request_get_onreadystatechange;
webkit_dom_xml_http_request_get_onreadystatechange = (function(self)
                                                      {
                                                        return self["onreadystatechange"];
                                                      });
var webkit_dom_xml_http_request_get_ready_state;
webkit_dom_xml_http_request_get_ready_state = (function(self)
                                               {
                                                 return $hs_int(self["readyState"]);
                                               });
var webkit_dom_xml_http_request_set_as_blob;
webkit_dom_xml_http_request_set_as_blob = (function(self, val)
                                           {
                                             self["asBlob"] = ($hs_intToNumber(val) != 0);
                                           });
var webkit_dom_xml_http_request_get_as_blob;
webkit_dom_xml_http_request_get_as_blob = (function(self)
                                           {
                                             return $hs_int((self["asBlob"] ? 1 : 0));
                                           });
var webkit_dom_xml_http_request_set_with_credentials;
webkit_dom_xml_http_request_set_with_credentials = (function(self,
                                                             val)
                                                    {
                                                      self["withCredentials"] = ($hs_intToNumber(val)
                                                                                 !=
                                                                                 0);
                                                    });
var webkit_dom_xml_http_request_get_with_credentials;
webkit_dom_xml_http_request_get_with_credentials = (function(self)
                                                    {
                                                      return $hs_int((self["withCredentials"] ? 1 : 0));
                                                    });
var webkit_dom_xml_http_request_get_upload;
webkit_dom_xml_http_request_get_upload = (function(self)
                                          {
                                            return self["upload"];
                                          });
var webkit_dom_xml_http_request_get_response_xml;
webkit_dom_xml_http_request_get_response_xml = (function(self)
                                                {
                                                  return self["responseXML"];
                                                });
var webkit_dom_xml_http_request_get_response_blob;
webkit_dom_xml_http_request_get_response_blob = (function(self)
                                                 {
                                                   return self["responseBlob"];
                                                 });
var webkit_dom_xml_http_request_set_response_type;
webkit_dom_xml_http_request_set_response_type = (function(self,
                                                          val)
                                                 {
                                                   self["responseType"] = $hs_fromUtf8(val);
                                                 });
var webkit_dom_xml_http_request_get_response_type;
webkit_dom_xml_http_request_get_response_type = (function(self)
                                                 {
                                                   return $hs_toUtf8(self["responseType"]);
                                                 });
var webkit_dom_xml_http_request_get_status;
webkit_dom_xml_http_request_get_status = (function(self)
                                          {
                                            return $hs_int(self["status"]);
                                          });
var webkit_dom_xml_http_request_get_status_text;
webkit_dom_xml_http_request_get_status_text = (function(self)
                                               {
                                                 return $hs_toUtf8(self["statusText"]);
                                               });
// Graphics.UI.Gtk.WebKit.DOM.Window
webkit_dom_webkit_point_get_type = (function()
                                    {
                                      return WebKitPoint;
                                    });
var webkit_dom_webkit_point_set_x;
webkit_dom_webkit_point_set_x = (function(self, val)
                                 {
                                   self["x"] = val;
                                 });
var webkit_dom_webkit_point_get_x;
webkit_dom_webkit_point_get_x = (function(self)
                                 {
                                   return self["x"];
                                 });
var webkit_dom_webkit_point_set_y;
webkit_dom_webkit_point_set_y = (function(self, val)
                                 {
                                   self["y"] = val;
                                 });
var webkit_dom_webkit_point_get_y;
webkit_dom_webkit_point_get_y = (function(self)
                                 {
                                   return self["y"];
                                 });
// Graphics.UI.Gtk.WebKit.DOM.Core
webkit_dom_webkit_named_flow_get_type = (function()
                                         {
                                           return WebKitNamedFlow;
                                         });
var webkit_dom_webkit_named_flow_get_overflow;
webkit_dom_webkit_named_flow_get_overflow = (function(self)
                                             {
                                               return $hs_int((self["overflow"] ? 1 : 0));
                                             });
// Graphics.UI.Gtk.WebKit.DOM.Events
webkit_dom_ui_event_get_type = (function()
                                {
                                  return UIEvent;
                                });
var webkit_dom_ui_event_init_ui_event;
webkit_dom_ui_event_init_ui_event = (function(self, type,
                                              canBubble, cancelable, view, detail)
                                     {
                                       return self["initUIEvent"]($hs_fromUtf8(type),
                                                                  ($hs_intToNumber(canBubble) != 0),
                                                                  ($hs_intToNumber(cancelable)
                                                                   !=
                                                                   0),
                                                                  view, $hs_intToNumber(detail));
                                     });
var webkit_dom_ui_event_get_view;
webkit_dom_ui_event_get_view = (function(self)
                                {
                                  return self["view"];
                                });
var webkit_dom_ui_event_get_detail;
webkit_dom_ui_event_get_detail = (function(self)
                                  {
                                    return $hs_int(self["detail"]);
                                  });
var webkit_dom_ui_event_get_key_code;
webkit_dom_ui_event_get_key_code = (function(self)
                                    {
                                      return $hs_int(self["keyCode"]);
                                    });
var webkit_dom_ui_event_get_char_code;
webkit_dom_ui_event_get_char_code = (function(self)
                                     {
                                       return $hs_int(self["charCode"]);
                                     });
var webkit_dom_ui_event_get_layer_x;
webkit_dom_ui_event_get_layer_x = (function(self)
                                   {
                                     return $hs_int(self["layerX"]);
                                   });
var webkit_dom_ui_event_get_layer_y;
webkit_dom_ui_event_get_layer_y = (function(self)
                                   {
                                     return $hs_int(self["layerY"]);
                                   });
var webkit_dom_ui_event_get_page_x;
webkit_dom_ui_event_get_page_x = (function(self)
                                  {
                                    return $hs_int(self["pageX"]);
                                  });
var webkit_dom_ui_event_get_page_y;
webkit_dom_ui_event_get_page_y = (function(self)
                                  {
                                    return $hs_int(self["pageY"]);
                                  });
var webkit_dom_ui_event_get_which;
webkit_dom_ui_event_get_which = (function(self)
                                 {
                                   return $hs_int(self["which"]);
                                 });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_validity_state_get_type = (function()
                                      {
                                        return ValidityState;
                                      });
var webkit_dom_validity_state_get_value_missing;
webkit_dom_validity_state_get_value_missing = (function(self)
                                               {
                                                 return $hs_int((self["valueMissing"] ? 1 : 0));
                                               });
var webkit_dom_validity_state_get_type_mismatch;
webkit_dom_validity_state_get_type_mismatch = (function(self)
                                               {
                                                 return $hs_int((self["typeMismatch"] ? 1 : 0));
                                               });
var webkit_dom_validity_state_get_pattern_mismatch;
webkit_dom_validity_state_get_pattern_mismatch = (function(self)
                                                  {
                                                    return $hs_int((self["patternMismatch"] ? 1 : 0));
                                                  });
var webkit_dom_validity_state_get_too_long;
webkit_dom_validity_state_get_too_long = (function(self)
                                          {
                                            return $hs_int((self["tooLong"] ? 1 : 0));
                                          });
var webkit_dom_validity_state_get_range_underflow;
webkit_dom_validity_state_get_range_underflow = (function(self)
                                                 {
                                                   return $hs_int((self["rangeUnderflow"] ? 1 : 0));
                                                 });
var webkit_dom_validity_state_get_range_overflow;
webkit_dom_validity_state_get_range_overflow = (function(self)
                                                {
                                                  return $hs_int((self["rangeOverflow"] ? 1 : 0));
                                                });
var webkit_dom_validity_state_get_step_mismatch;
webkit_dom_validity_state_get_step_mismatch = (function(self)
                                               {
                                                 return $hs_int((self["stepMismatch"] ? 1 : 0));
                                               });
var webkit_dom_validity_state_get_custom_error;
webkit_dom_validity_state_get_custom_error = (function(self)
                                              {
                                                return $hs_int((self["customError"] ? 1 : 0));
                                              });
var webkit_dom_validity_state_get_valid;
webkit_dom_validity_state_get_valid = (function(self)
                                       {
                                         return $hs_int((self["valid"] ? 1 : 0));
                                       });
// Graphics.UI.Gtk.WebKit.DOM.Traversal
webkit_dom_tree_walker_get_type = (function()
                                   {
                                     return TreeWalker;
                                   });
var webkit_dom_tree_walker_get_root;
webkit_dom_tree_walker_get_root = (function(self)
                                   {
                                     return self["root"];
                                   });
var webkit_dom_tree_walker_get_what_to_show;
webkit_dom_tree_walker_get_what_to_show = (function(self)
                                           {
                                             return $hs_int(self["whatToShow"]);
                                           });
var webkit_dom_tree_walker_get_filter;
webkit_dom_tree_walker_get_filter = (function(self)
                                     {
                                       return self["filter"];
                                     });
var webkit_dom_tree_walker_get_expand_entity_references;
webkit_dom_tree_walker_get_expand_entity_references = (function(self)
                                                       {
                                                         return $hs_int((self["expandEntityReferences"] ? 1 : 0));
                                                       });
var webkit_dom_tree_walker_set_current_node;
webkit_dom_tree_walker_set_current_node = (function(self, val)
                                           {
                                             self["currentNode"] = val;
                                           });
var webkit_dom_tree_walker_get_current_node;
webkit_dom_tree_walker_get_current_node = (function(self)
                                           {
                                             return self["currentNode"];
                                           });
// Graphics.UI.Gtk.WebKit.DOM.Core
webkit_dom_text_get_type = (function()
                            {
                              return Text;
                            });
var webkit_dom_text_split_text;
webkit_dom_text_split_text = (function(self, offset)
                              {
                                return self["splitText"]($hs_intToNumber(offset));
                              });
var webkit_dom_text_replace_whole_text;
webkit_dom_text_replace_whole_text = (function(self, content)
                                      {
                                        return self["replaceWholeText"]($hs_fromUtf8(content));
                                      });
var webkit_dom_text_get_whole_text;
webkit_dom_text_get_whole_text = (function(self)
                                  {
                                    return $hs_toUtf8(self["wholeText"]);
                                  });
// Graphics.UI.Gtk.WebKit.DOM.Storage
webkit_dom_storage_get_type = (function()
                               {
                                 return Storage;
                               });
var webkit_dom_storage_key;
webkit_dom_storage_key = (function(self, index)
                          {
                            return $hs_toUtf8(self["key"]($hs_intToNumber(index)));
                          });
var webkit_dom_storage_get_item;
webkit_dom_storage_get_item = (function(self, key)
                               {
                                 return $hs_toUtf8(self["getItem"]($hs_fromUtf8(key)));
                               });
var webkit_dom_storage_set_item;
webkit_dom_storage_set_item = (function(self, key, data)
                               {
                                 return self["setItem"]($hs_fromUtf8(key), $hs_fromUtf8(data));
                               });
var webkit_dom_storage_remove_item;
webkit_dom_storage_remove_item = (function(self, key)
                                  {
                                    return self["removeItem"]($hs_fromUtf8(key));
                                  });
var webkit_dom_storage_clear;
webkit_dom_storage_clear = (function(self)
                            {
                              return self["clear"]();
                            });
var webkit_dom_storage_get_length;
webkit_dom_storage_get_length = (function(self)
                                 {
                                   return $hs_int(self["length"]);
                                 });
// Graphics.UI.Gtk.WebKit.DOM.Stylesheets
webkit_dom_style_sheet_list_get_type = (function()
                                        {
                                          return StyleSheetList;
                                        });
var webkit_dom_style_sheet_list_item;
webkit_dom_style_sheet_list_item = (function(self, index)
                                    {
                                      return self["item"]($hs_intToNumber(index));
                                    });
var webkit_dom_style_sheet_list_get_length;
webkit_dom_style_sheet_list_get_length = (function(self)
                                          {
                                            return $hs_int(self["length"]);
                                          });
// Graphics.UI.Gtk.WebKit.DOM.Stylesheets
webkit_dom_style_sheet_get_type = (function()
                                   {
                                     return StyleSheet;
                                   });
var webkit_dom_style_sheet_set_disabled;
webkit_dom_style_sheet_set_disabled = (function(self, val)
                                       {
                                         self["disabled"] = ($hs_intToNumber(val) != 0);
                                       });
var webkit_dom_style_sheet_get_disabled;
webkit_dom_style_sheet_get_disabled = (function(self)
                                       {
                                         return $hs_int((self["disabled"] ? 1 : 0));
                                       });
var webkit_dom_style_sheet_get_owner_node;
webkit_dom_style_sheet_get_owner_node = (function(self)
                                         {
                                           return self["ownerNode"];
                                         });
var webkit_dom_style_sheet_get_parent_style_sheet;
webkit_dom_style_sheet_get_parent_style_sheet = (function(self)
                                                 {
                                                   return self["parentStyleSheet"];
                                                 });
var webkit_dom_style_sheet_get_href;
webkit_dom_style_sheet_get_href = (function(self)
                                   {
                                     return $hs_toUtf8(self["href"]);
                                   });
var webkit_dom_style_sheet_get_title;
webkit_dom_style_sheet_get_title = (function(self)
                                    {
                                      return $hs_toUtf8(self["title"]);
                                    });
var webkit_dom_style_sheet_get_media;
webkit_dom_style_sheet_get_media = (function(self)
                                    {
                                      return self["media"];
                                    });
// Graphics.UI.Gtk.WebKit.DOM.View
webkit_dom_style_media_get_type = (function()
                                   {
                                     return StyleMedia;
                                   });
var webkit_dom_style_media_match_medium;
webkit_dom_style_media_match_medium = (function(self, mediaquery)
                                       {
                                         return $hs_int((self["matchMedium"]($hs_fromUtf8(mediaquery)) ? 1 : 0));
                                       });
// Graphics.UI.Gtk.WebKit.DOM.Window
webkit_dom_screen_get_type = (function()
                              {
                                return Screen;
                              });
var webkit_dom_screen_get_height;
webkit_dom_screen_get_height = (function(self)
                                {
                                  return $hs_int(self["height"]);
                                });
var webkit_dom_screen_get_width;
webkit_dom_screen_get_width = (function(self)
                               {
                                 return $hs_int(self["width"]);
                               });
var webkit_dom_screen_get_color_depth;
webkit_dom_screen_get_color_depth = (function(self)
                                     {
                                       return $hs_int(self["colorDepth"]);
                                     });
var webkit_dom_screen_get_pixel_depth;
webkit_dom_screen_get_pixel_depth = (function(self)
                                     {
                                       return $hs_int(self["pixelDepth"]);
                                     });
var webkit_dom_screen_get_avail_left;
webkit_dom_screen_get_avail_left = (function(self)
                                    {
                                      return $hs_int(self["availLeft"]);
                                    });
var webkit_dom_screen_get_avail_top;
webkit_dom_screen_get_avail_top = (function(self)
                                   {
                                     return $hs_int(self["availTop"]);
                                   });
var webkit_dom_screen_get_avail_height;
webkit_dom_screen_get_avail_height = (function(self)
                                      {
                                        return $hs_int(self["availHeight"]);
                                      });
var webkit_dom_screen_get_avail_width;
webkit_dom_screen_get_avail_width = (function(self)
                                     {
                                       return $hs_int(self["availWidth"]);
                                     });
// Graphics.UI.Gtk.WebKit.DOM.Ranges
webkit_dom_range_get_type = (function()
                             {
                               return Range;
                             });
var webkit_dom_range_set_start;
webkit_dom_range_set_start = (function(self, refNode, offset)
                              {
                                return self["setStart"](refNode, $hs_intToNumber(offset));
                              });
var webkit_dom_range_set_end;
webkit_dom_range_set_end = (function(self, refNode, offset)
                            {
                              return self["setEnd"](refNode, $hs_intToNumber(offset));
                            });
var webkit_dom_range_set_start_before;
webkit_dom_range_set_start_before = (function(self, refNode)
                                     {
                                       return self["setStartBefore"](refNode);
                                     });
var webkit_dom_range_set_start_after;
webkit_dom_range_set_start_after = (function(self, refNode)
                                    {
                                      return self["setStartAfter"](refNode);
                                    });
var webkit_dom_range_set_end_before;
webkit_dom_range_set_end_before = (function(self, refNode)
                                   {
                                     return self["setEndBefore"](refNode);
                                   });
var webkit_dom_range_set_end_after;
webkit_dom_range_set_end_after = (function(self, refNode)
                                  {
                                    return self["setEndAfter"](refNode);
                                  });
var webkit_dom_range_collapse;
webkit_dom_range_collapse = (function(self, toStart)
                             {
                               return self["collapse"](($hs_intToNumber(toStart) != 0));
                             });
var webkit_dom_range_select_node;
webkit_dom_range_select_node = (function(self, refNode)
                                {
                                  return self["selectNode"](refNode);
                                });
var webkit_dom_range_select_node_contents;
webkit_dom_range_select_node_contents = (function(self, refNode)
                                         {
                                           return self["selectNodeContents"](refNode);
                                         });
var webkit_dom_range_compare_boundary_points;
webkit_dom_range_compare_boundary_points = (function(self, how,
                                                     sourceRange)
                                            {
                                              return $hs_int(self["compareBoundaryPoints"]($hs_intToNumber(how),
                                                                                           sourceRange));
                                            });
var webkit_dom_range_delete_contents;
webkit_dom_range_delete_contents = (function(self)
                                    {
                                      return self["deleteContents"]();
                                    });
var webkit_dom_range_extract_contents;
webkit_dom_range_extract_contents = (function(self)
                                     {
                                       return self["extractContents"]();
                                     });
var webkit_dom_range_clone_contents;
webkit_dom_range_clone_contents = (function(self)
                                   {
                                     return self["cloneContents"]();
                                   });
var webkit_dom_range_insert_node;
webkit_dom_range_insert_node = (function(self, newNode)
                                {
                                  return self["insertNode"](newNode);
                                });
var webkit_dom_range_surround_contents;
webkit_dom_range_surround_contents = (function(self, newParent)
                                      {
                                        return self["surroundContents"](newParent);
                                      });
var webkit_dom_range_clone_range;
webkit_dom_range_clone_range = (function(self)
                                {
                                  return self["cloneRange"]();
                                });
var webkit_dom_range_to_string;
webkit_dom_range_to_string = (function(self)
                              {
                                return $hs_toUtf8(self["toString"]());
                              });
var webkit_dom_range_detach;
webkit_dom_range_detach = (function(self)
                           {
                             return self["detach"]();
                           });
var webkit_dom_range_create_contextual_fragment;
webkit_dom_range_create_contextual_fragment = (function(self, html)
                                               {
                                                 return self["createContextualFragment"]($hs_fromUtf8(html));
                                               });
var webkit_dom_range_intersects_node;
webkit_dom_range_intersects_node = (function(self, refNode)
                                    {
                                      return $hs_int((self["intersectsNode"](refNode) ? 1 : 0));
                                    });
var webkit_dom_range_compare_node;
webkit_dom_range_compare_node = (function(self, refNode)
                                 {
                                   return $hs_int(self["compareNode"](refNode));
                                 });
var webkit_dom_range_compare_point;
webkit_dom_range_compare_point = (function(self, refNode, offset)
                                  {
                                    return $hs_int(self["comparePoint"](refNode,
                                                                        $hs_intToNumber(offset)));
                                  });
var webkit_dom_range_is_point_in_range;
webkit_dom_range_is_point_in_range = (function(self, refNode,
                                               offset)
                                      {
                                        return $hs_int((self["isPointInRange"](refNode,
                                                                               $hs_intToNumber(offset)) ? 1 : 0));
                                      });
var webkit_dom_range_expand;
webkit_dom_range_expand = (function(self, unit)
                           {
                             return self["expand"]($hs_fromUtf8(unit));
                           });
var webkit_dom_range_get_start_container;
webkit_dom_range_get_start_container = (function(self)
                                        {
                                          return self["startContainer"];
                                        });
var webkit_dom_range_get_start_offset;
webkit_dom_range_get_start_offset = (function(self)
                                     {
                                       return $hs_int(self["startOffset"]);
                                     });
var webkit_dom_range_get_end_container;
webkit_dom_range_get_end_container = (function(self)
                                      {
                                        return self["endContainer"];
                                      });
var webkit_dom_range_get_end_offset;
webkit_dom_range_get_end_offset = (function(self)
                                   {
                                     return $hs_int(self["endOffset"]);
                                   });
var webkit_dom_range_get_collapsed;
webkit_dom_range_get_collapsed = (function(self)
                                  {
                                    return $hs_int((self["collapsed"] ? 1 : 0));
                                  });
var webkit_dom_range_get_common_ancestor_container;
webkit_dom_range_get_common_ancestor_container = (function(self)
                                                  {
                                                    return self["commonAncestorContainer"];
                                                  });
var webkit_dom_range_get_text;
webkit_dom_range_get_text = (function(self)
                             {
                               return $hs_toUtf8(self["text"]);
                             });
// Graphics.UI.Gtk.WebKit.DOM.Core
webkit_dom_processing_instruction_get_type = (function()
                                              {
                                                return ProcessingInstruction;
                                              });
var webkit_dom_processing_instruction_get_target;
webkit_dom_processing_instruction_get_target = (function(self)
                                                {
                                                  return $hs_toUtf8(self["target"]);
                                                });
var webkit_dom_processing_instruction_set_data;
webkit_dom_processing_instruction_set_data = (function(self, val)
                                              {
                                                self["data"] = $hs_fromUtf8(val);
                                              });
var webkit_dom_processing_instruction_get_data;
webkit_dom_processing_instruction_get_data = (function(self)
                                              {
                                                return $hs_toUtf8(self["data"]);
                                              });
var webkit_dom_processing_instruction_get_sheet;
webkit_dom_processing_instruction_get_sheet = (function(self)
                                               {
                                                 return self["sheet"];
                                               });
// Graphics.UI.Gtk.WebKit.DOM.Core
webkit_dom_notation_get_type = (function()
                                {
                                  return Notation;
                                });
var webkit_dom_notation_get_public_id;
webkit_dom_notation_get_public_id = (function(self)
                                     {
                                       return $hs_toUtf8(self["publicId"]);
                                     });
var webkit_dom_notation_get_system_id;
webkit_dom_notation_get_system_id = (function(self)
                                     {
                                       return $hs_toUtf8(self["systemId"]);
                                     });
// Graphics.UI.Gtk.WebKit.DOM.Core
webkit_dom_node_list_get_type = (function()
                                 {
                                   return NodeList;
                                 });
var webkit_dom_node_list_item;
webkit_dom_node_list_item = (function(self, index)
                             {
                               return self["item"]($hs_intToNumber(index));
                             });
var webkit_dom_node_list_get_length;
webkit_dom_node_list_get_length = (function(self)
                                   {
                                     return $hs_int(self["length"]);
                                   });
// Graphics.UI.Gtk.WebKit.DOM.Traversal
webkit_dom_node_iterator_get_type = (function()
                                     {
                                       return NodeIterator;
                                     });
var webkit_dom_node_iterator_detach;
webkit_dom_node_iterator_detach = (function(self)
                                   {
                                     return self["detach"]();
                                   });
var webkit_dom_node_iterator_get_root;
webkit_dom_node_iterator_get_root = (function(self)
                                     {
                                       return self["root"];
                                     });
var webkit_dom_node_iterator_get_what_to_show;
webkit_dom_node_iterator_get_what_to_show = (function(self)
                                             {
                                               return $hs_int(self["whatToShow"]);
                                             });
var webkit_dom_node_iterator_get_filter;
webkit_dom_node_iterator_get_filter = (function(self)
                                       {
                                         return self["filter"];
                                       });
var webkit_dom_node_iterator_get_expand_entity_references;
webkit_dom_node_iterator_get_expand_entity_references = (function(self)
                                                         {
                                                           return $hs_int((self["expandEntityReferences"] ? 1 : 0));
                                                         });
var webkit_dom_node_iterator_get_reference_node;
webkit_dom_node_iterator_get_reference_node = (function(self)
                                               {
                                                 return self["referenceNode"];
                                               });
var webkit_dom_node_iterator_get_pointer_before_reference_node;
webkit_dom_node_iterator_get_pointer_before_reference_node = (function(self)
                                                              {
                                                                return $hs_int((self["pointerBeforeReferenceNode"] ? 1 : 0));
                                                              });
// Graphics.UI.Gtk.WebKit.DOM.Traversal
webkit_dom_node_filter_get_type = (function()
                                   {
                                     return NodeFilter;
                                   });
// Graphics.UI.Gtk.WebKit.DOM.Core
webkit_dom_node_get_type = (function()
                            {
                              return Node;
                            });
var webkit_dom_node_insert_before;
webkit_dom_node_insert_before = (function(self, newChild, refChild)
                                 {
                                   return self["insertBefore"](newChild, refChild);
                                 });
var webkit_dom_node_replace_child;
webkit_dom_node_replace_child = (function(self, newChild, oldChild)
                                 {
                                   return self["replaceChild"](newChild, oldChild);
                                 });
var webkit_dom_node_remove_child;
webkit_dom_node_remove_child = (function(self, oldChild)
                                {
                                  return self["removeChild"](oldChild);
                                });
var webkit_dom_node_append_child;
webkit_dom_node_append_child = (function(self, newChild)
                                {
                                  return self["appendChild"](newChild);
                                });
var webkit_dom_node_has_child_nodes;
webkit_dom_node_has_child_nodes = (function(self)
                                   {
                                     return $hs_int((self["hasChildNodes"]() ? 1 : 0));
                                   });
var webkit_dom_node_clone_node;
webkit_dom_node_clone_node = (function(self, deep)
                              {
                                return self["cloneNode"](($hs_intToNumber(deep) != 0));
                              });
var webkit_dom_node_normalize;
webkit_dom_node_normalize = (function(self)
                             {
                               return self["normalize"]();
                             });
var webkit_dom_node_is_supported;
webkit_dom_node_is_supported = (function(self, feature, version)
                                {
                                  return $hs_int((self["isSupported"]($hs_fromUtf8(feature),
                                                                      $hs_fromUtf8(version)) ? 1 : 0));
                                });
var webkit_dom_node_has_attributes;
webkit_dom_node_has_attributes = (function(self)
                                  {
                                    return $hs_int((self["hasAttributes"]() ? 1 : 0));
                                  });
var webkit_dom_node_is_same_node;
webkit_dom_node_is_same_node = (function(self, other)
                                {
                                  return $hs_int((self["isSameNode"](other) ? 1 : 0));
                                });
var webkit_dom_node_is_equal_node;
webkit_dom_node_is_equal_node = (function(self, other)
                                 {
                                   return $hs_int((self["isEqualNode"](other) ? 1 : 0));
                                 });
var webkit_dom_node_lookup_prefix;
webkit_dom_node_lookup_prefix = (function(self, namespaceURI)
                                 {
                                   return $hs_toUtf8(self["lookupPrefix"]($hs_fromUtf8(namespaceURI)));
                                 });
var webkit_dom_node_is_default_namespace;
webkit_dom_node_is_default_namespace = (function(self,
                                                 namespaceURI)
                                        {
                                          return $hs_int((self["isDefaultNamespace"]($hs_fromUtf8(namespaceURI)) ? 1 : 0));
                                        });
var webkit_dom_node_lookup_namespace_uri;
webkit_dom_node_lookup_namespace_uri = (function(self, prefix)
                                        {
                                          return $hs_toUtf8(self["lookupNamespaceURI"]($hs_fromUtf8(prefix)));
                                        });
var webkit_dom_node_compare_document_position;
webkit_dom_node_compare_document_position = (function(self, other)
                                             {
                                               return $hs_int(self["compareDocumentPosition"](other));
                                             });
var webkit_dom_node_contains;
webkit_dom_node_contains = (function(self, other)
                            {
                              return $hs_int((self["contains"](other) ? 1 : 0));
                            });
var webkit_dom_node_dispatch_event;
webkit_dom_node_dispatch_event = (function(self, event)
                                  {
                                    return $hs_int((self["dispatchEvent"](event) ? 1 : 0));
                                  });
var webkit_dom_node_get_node_name;
webkit_dom_node_get_node_name = (function(self)
                                 {
                                   return $hs_toUtf8(self["nodeName"]);
                                 });
var webkit_dom_node_set_node_value;
webkit_dom_node_set_node_value = (function(self, val)
                                  {
                                    self["nodeValue"] = $hs_fromUtf8(val);
                                  });
var webkit_dom_node_get_node_value;
webkit_dom_node_get_node_value = (function(self)
                                  {
                                    return $hs_toUtf8(self["nodeValue"]);
                                  });
var webkit_dom_node_get_node_type;
webkit_dom_node_get_node_type = (function(self)
                                 {
                                   return $hs_int(self["nodeType"]);
                                 });
var webkit_dom_node_get_parent_node;
webkit_dom_node_get_parent_node = (function(self)
                                   {
                                     return self["parentNode"];
                                   });
var webkit_dom_node_get_child_nodes;
webkit_dom_node_get_child_nodes = (function(self)
                                   {
                                     return self["childNodes"];
                                   });
var webkit_dom_node_get_first_child;
webkit_dom_node_get_first_child = (function(self)
                                   {
                                     return self["firstChild"];
                                   });
var webkit_dom_node_get_last_child;
webkit_dom_node_get_last_child = (function(self)
                                  {
                                    return self["lastChild"];
                                  });
var webkit_dom_node_get_previous_sibling;
webkit_dom_node_get_previous_sibling = (function(self)
                                        {
                                          return self["previousSibling"];
                                        });
var webkit_dom_node_get_next_sibling;
webkit_dom_node_get_next_sibling = (function(self)
                                    {
                                      return self["nextSibling"];
                                    });
var webkit_dom_node_get_attributes;
webkit_dom_node_get_attributes = (function(self)
                                  {
                                    return self["attributes"];
                                  });
var webkit_dom_node_get_owner_document;
webkit_dom_node_get_owner_document = (function(self)
                                      {
                                        return self["ownerDocument"];
                                      });
var webkit_dom_node_get_namespace_uri;
webkit_dom_node_get_namespace_uri = (function(self)
                                     {
                                       return $hs_toUtf8(self["namespaceURI"]);
                                     });
var webkit_dom_node_set_prefix;
webkit_dom_node_set_prefix = (function(self, val)
                              {
                                self["prefix"] = $hs_fromUtf8(val);
                              });
var webkit_dom_node_get_prefix;
webkit_dom_node_get_prefix = (function(self)
                              {
                                return $hs_toUtf8(self["prefix"]);
                              });
var webkit_dom_node_get_local_name;
webkit_dom_node_get_local_name = (function(self)
                                  {
                                    return $hs_toUtf8(self["localName"]);
                                  });
var webkit_dom_node_get_base_uri;
webkit_dom_node_get_base_uri = (function(self)
                                {
                                  return $hs_toUtf8(self["baseURI"]);
                                });
var webkit_dom_node_set_text_content;
webkit_dom_node_set_text_content = (function(self, val)
                                    {
                                      self["textContent"] = $hs_fromUtf8(val);
                                    });
var webkit_dom_node_get_text_content;
webkit_dom_node_get_text_content = (function(self)
                                    {
                                      return $hs_toUtf8(self["textContent"]);
                                    });
var webkit_dom_node_get_parent_element;
webkit_dom_node_get_parent_element = (function(self)
                                      {
                                        return self["parentElement"];
                                      });
// Graphics.UI.Gtk.WebKit.DOM.Window
webkit_dom_navigator_get_type = (function()
                                 {
                                   return Navigator;
                                 });
var webkit_dom_navigator_java_enabled;
webkit_dom_navigator_java_enabled = (function(self)
                                     {
                                       return $hs_int((self["javaEnabled"]() ? 1 : 0));
                                     });
var webkit_dom_navigator_get_storage_updates;
webkit_dom_navigator_get_storage_updates = (function(self)
                                            {
                                              return self["getStorageUpdates"]();
                                            });
var webkit_dom_navigator_get_app_code_name;
webkit_dom_navigator_get_app_code_name = (function(self)
                                          {
                                            return $hs_toUtf8(self["appCodeName"]);
                                          });
var webkit_dom_navigator_get_app_name;
webkit_dom_navigator_get_app_name = (function(self)
                                     {
                                       return $hs_toUtf8(self["appName"]);
                                     });
var webkit_dom_navigator_get_app_version;
webkit_dom_navigator_get_app_version = (function(self)
                                        {
                                          return $hs_toUtf8(self["appVersion"]);
                                        });
var webkit_dom_navigator_get_language;
webkit_dom_navigator_get_language = (function(self)
                                     {
                                       return $hs_toUtf8(self["language"]);
                                     });
var webkit_dom_navigator_get_user_agent;
webkit_dom_navigator_get_user_agent = (function(self)
                                       {
                                         return $hs_toUtf8(self["userAgent"]);
                                       });
var webkit_dom_navigator_get_platform;
webkit_dom_navigator_get_platform = (function(self)
                                     {
                                       return $hs_toUtf8(self["platform"]);
                                     });
var webkit_dom_navigator_get_plugins;
webkit_dom_navigator_get_plugins = (function(self)
                                    {
                                      return self["plugins"];
                                    });
var webkit_dom_navigator_get_mime_types;
webkit_dom_navigator_get_mime_types = (function(self)
                                       {
                                         return self["mimeTypes"];
                                       });
var webkit_dom_navigator_get_product;
webkit_dom_navigator_get_product = (function(self)
                                    {
                                      return $hs_toUtf8(self["product"]);
                                    });
var webkit_dom_navigator_get_product_sub;
webkit_dom_navigator_get_product_sub = (function(self)
                                        {
                                          return $hs_toUtf8(self["productSub"]);
                                        });
var webkit_dom_navigator_get_vendor;
webkit_dom_navigator_get_vendor = (function(self)
                                   {
                                     return $hs_toUtf8(self["vendor"]);
                                   });
var webkit_dom_navigator_get_vendor_sub;
webkit_dom_navigator_get_vendor_sub = (function(self)
                                       {
                                         return $hs_toUtf8(self["vendorSub"]);
                                       });
var webkit_dom_navigator_get_cookie_enabled;
webkit_dom_navigator_get_cookie_enabled = (function(self)
                                           {
                                             return $hs_int((self["cookieEnabled"] ? 1 : 0));
                                           });
var webkit_dom_navigator_get_on_line;
webkit_dom_navigator_get_on_line = (function(self)
                                    {
                                      return $hs_int((self["onLine"] ? 1 : 0));
                                    });
// Graphics.UI.Gtk.WebKit.DOM.Core
webkit_dom_named_node_map_get_type = (function()
                                      {
                                        return NamedNodeMap;
                                      });
var webkit_dom_named_node_map_get_named_item;
webkit_dom_named_node_map_get_named_item = (function(self, name)
                                            {
                                              return self["getNamedItem"]($hs_fromUtf8(name));
                                            });
var webkit_dom_named_node_map_set_named_item;
webkit_dom_named_node_map_set_named_item = (function(self, node)
                                            {
                                              return self["setNamedItem"](node);
                                            });
var webkit_dom_named_node_map_remove_named_item;
webkit_dom_named_node_map_remove_named_item = (function(self, name)
                                               {
                                                 return self["removeNamedItem"]($hs_fromUtf8(name));
                                               });
var webkit_dom_named_node_map_item;
webkit_dom_named_node_map_item = (function(self, index)
                                  {
                                    return self["item"]($hs_intToNumber(index));
                                  });
var webkit_dom_named_node_map_get_named_item_ns;
webkit_dom_named_node_map_get_named_item_ns = (function(self,
                                                        namespaceURI, localName)
                                               {
                                                 return self["getNamedItemNS"]($hs_fromUtf8(namespaceURI),
                                                                               $hs_fromUtf8(localName));
                                               });
var webkit_dom_named_node_map_set_named_item_ns;
webkit_dom_named_node_map_set_named_item_ns = (function(self, node)
                                               {
                                                 return self["setNamedItemNS"](node);
                                               });
var webkit_dom_named_node_map_remove_named_item_ns;
webkit_dom_named_node_map_remove_named_item_ns = (function(self,
                                                           namespaceURI, localName)
                                                  {
                                                    return self["removeNamedItemNS"]($hs_fromUtf8(namespaceURI),
                                                                                     $hs_fromUtf8(localName));
                                                  });
var webkit_dom_named_node_map_get_length;
webkit_dom_named_node_map_get_length = (function(self)
                                        {
                                          return $hs_int(self["length"]);
                                        });
// Graphics.UI.Gtk.WebKit.DOM.Events
webkit_dom_mutation_event_get_type = (function()
                                      {
                                        return MutationEvent;
                                      });
var webkit_dom_mutation_event_init_mutation_event;
webkit_dom_mutation_event_init_mutation_event = (function(self,
                                                          type, canBubble, cancelable, relatedNode,
                                                          prevValue, newValue, attrName, attrChange)
                                                 {
                                                   return self["initMutationEvent"]($hs_fromUtf8(type),
                                                                                    ($hs_intToNumber(canBubble)
                                                                                     !=
                                                                                     0),
                                                                                    ($hs_intToNumber(cancelable)
                                                                                     !=
                                                                                     0),
                                                                                    relatedNode,
                                                                                    $hs_fromUtf8(prevValue),
                                                                                    $hs_fromUtf8(newValue),
                                                                                    $hs_fromUtf8(attrName),
                                                                                    $hs_intToNumber(attrChange));
                                                 });
var webkit_dom_mutation_event_get_related_node;
webkit_dom_mutation_event_get_related_node = (function(self)
                                              {
                                                return self["relatedNode"];
                                              });
var webkit_dom_mutation_event_get_prev_value;
webkit_dom_mutation_event_get_prev_value = (function(self)
                                            {
                                              return $hs_toUtf8(self["prevValue"]);
                                            });
var webkit_dom_mutation_event_get_new_value;
webkit_dom_mutation_event_get_new_value = (function(self)
                                           {
                                             return $hs_toUtf8(self["newValue"]);
                                           });
var webkit_dom_mutation_event_get_attr_name;
webkit_dom_mutation_event_get_attr_name = (function(self)
                                           {
                                             return $hs_toUtf8(self["attrName"]);
                                           });
var webkit_dom_mutation_event_get_attr_change;
webkit_dom_mutation_event_get_attr_change = (function(self)
                                             {
                                               return $hs_int(self["attrChange"]);
                                             });
// Graphics.UI.Gtk.WebKit.DOM.Events
webkit_dom_mouse_event_get_type = (function()
                                   {
                                     return MouseEvent;
                                   });
var webkit_dom_mouse_event_init_mouse_event;
webkit_dom_mouse_event_init_mouse_event = (function(self, type,
                                                    canBubble, cancelable, view, detail, screenX,
                                                    screenY, clientX, clientY, ctrlKey, altKey,
                                                    shiftKey, metaKey, button, relatedTarget)
                                           {
                                             return self["initMouseEvent"]($hs_fromUtf8(type),
                                                                           ($hs_intToNumber(canBubble)
                                                                            !=
                                                                            0),
                                                                           ($hs_intToNumber(cancelable)
                                                                            !=
                                                                            0),
                                                                           view,
                                                                           $hs_intToNumber(detail),
                                                                           $hs_intToNumber(screenX),
                                                                           $hs_intToNumber(screenY),
                                                                           $hs_intToNumber(clientX),
                                                                           $hs_intToNumber(clientY),
                                                                           ($hs_intToNumber(ctrlKey)
                                                                            !=
                                                                            0),
                                                                           ($hs_intToNumber(altKey)
                                                                            !=
                                                                            0),
                                                                           ($hs_intToNumber(shiftKey)
                                                                            !=
                                                                            0),
                                                                           ($hs_intToNumber(metaKey)
                                                                            !=
                                                                            0),
                                                                           $hs_intToNumber(button),
                                                                           relatedTarget);
                                           });
var webkit_dom_mouse_event_get_screen_x;
webkit_dom_mouse_event_get_screen_x = (function(self)
                                       {
                                         return $hs_int(self["screenX"]);
                                       });
var webkit_dom_mouse_event_get_screen_y;
webkit_dom_mouse_event_get_screen_y = (function(self)
                                       {
                                         return $hs_int(self["screenY"]);
                                       });
var webkit_dom_mouse_event_get_client_x;
webkit_dom_mouse_event_get_client_x = (function(self)
                                       {
                                         return $hs_int(self["clientX"]);
                                       });
var webkit_dom_mouse_event_get_client_y;
webkit_dom_mouse_event_get_client_y = (function(self)
                                       {
                                         return $hs_int(self["clientY"]);
                                       });
var webkit_dom_mouse_event_get_webkit_movement_x;
webkit_dom_mouse_event_get_webkit_movement_x = (function(self)
                                                {
                                                  return $hs_int(self["webkitMovementX"]);
                                                });
var webkit_dom_mouse_event_get_webkit_movement_y;
webkit_dom_mouse_event_get_webkit_movement_y = (function(self)
                                                {
                                                  return $hs_int(self["webkitMovementY"]);
                                                });
var webkit_dom_mouse_event_get_ctrl_key;
webkit_dom_mouse_event_get_ctrl_key = (function(self)
                                       {
                                         return $hs_int((self["ctrlKey"] ? 1 : 0));
                                       });
var webkit_dom_mouse_event_get_shift_key;
webkit_dom_mouse_event_get_shift_key = (function(self)
                                        {
                                          return $hs_int((self["shiftKey"] ? 1 : 0));
                                        });
var webkit_dom_mouse_event_get_alt_key;
webkit_dom_mouse_event_get_alt_key = (function(self)
                                      {
                                        return $hs_int((self["altKey"] ? 1 : 0));
                                      });
var webkit_dom_mouse_event_get_meta_key;
webkit_dom_mouse_event_get_meta_key = (function(self)
                                       {
                                         return $hs_int((self["metaKey"] ? 1 : 0));
                                       });
var webkit_dom_mouse_event_get_button;
webkit_dom_mouse_event_get_button = (function(self)
                                     {
                                       return $hs_int(self["button"]);
                                     });
var webkit_dom_mouse_event_get_related_target;
webkit_dom_mouse_event_get_related_target = (function(self)
                                             {
                                               return self["relatedTarget"];
                                             });
var webkit_dom_mouse_event_get_offset_x;
webkit_dom_mouse_event_get_offset_x = (function(self)
                                       {
                                         return $hs_int(self["offsetX"]);
                                       });
var webkit_dom_mouse_event_get_offset_y;
webkit_dom_mouse_event_get_offset_y = (function(self)
                                       {
                                         return $hs_int(self["offsetY"]);
                                       });
var webkit_dom_mouse_event_get_x;
webkit_dom_mouse_event_get_x = (function(self)
                                {
                                  return $hs_int(self["x"]);
                                });
var webkit_dom_mouse_event_get_y;
webkit_dom_mouse_event_get_y = (function(self)
                                {
                                  return $hs_int(self["y"]);
                                });
var webkit_dom_mouse_event_get_from_element;
webkit_dom_mouse_event_get_from_element = (function(self)
                                           {
                                             return self["fromElement"];
                                           });
var webkit_dom_mouse_event_get_to_element;
webkit_dom_mouse_event_get_to_element = (function(self)
                                         {
                                           return self["toElement"];
                                         });
// Graphics.UI.Gtk.WebKit.DOM.Window
webkit_dom_memory_info_get_type = (function()
                                   {
                                     return MemoryInfo;
                                   });
var webkit_dom_memory_info_get_total_js_heap_size;
webkit_dom_memory_info_get_total_js_heap_size = (function(self)
                                                 {
                                                   return $hs_int(self["totalJSHeapSize"]);
                                                 });
var webkit_dom_memory_info_get_used_js_heap_size;
webkit_dom_memory_info_get_used_js_heap_size = (function(self)
                                                {
                                                  return $hs_int(self["usedJSHeapSize"]);
                                                });
var webkit_dom_memory_info_get_js_heap_size_limit;
webkit_dom_memory_info_get_js_heap_size_limit = (function(self)
                                                 {
                                                   return $hs_int(self["jsHeapSizeLimit"]);
                                                 });
// Graphics.UI.Gtk.WebKit.DOM.View
webkit_dom_media_query_list_get_type = (function()
                                        {
                                          return MediaQueryList;
                                        });
var webkit_dom_media_query_list_get_media;
webkit_dom_media_query_list_get_media = (function(self)
                                         {
                                           return $hs_toUtf8(self["media"]);
                                         });
var webkit_dom_media_query_list_get_matches;
webkit_dom_media_query_list_get_matches = (function(self)
                                           {
                                             return $hs_int((self["matches"] ? 1 : 0));
                                           });
// Graphics.UI.Gtk.WebKit.DOM.Stylesheets
webkit_dom_media_list_get_type = (function()
                                  {
                                    return MediaList;
                                  });
var webkit_dom_media_list_item;
webkit_dom_media_list_item = (function(self, index)
                              {
                                return $hs_toUtf8(self["item"]($hs_intToNumber(index)));
                              });
var webkit_dom_media_list_delete_medium;
webkit_dom_media_list_delete_medium = (function(self, oldMedium)
                                       {
                                         return self["deleteMedium"]($hs_fromUtf8(oldMedium));
                                       });
var webkit_dom_media_list_append_medium;
webkit_dom_media_list_append_medium = (function(self, newMedium)
                                       {
                                         return self["appendMedium"]($hs_fromUtf8(newMedium));
                                       });
var webkit_dom_media_list_set_media_text;
webkit_dom_media_list_set_media_text = (function(self, val)
                                        {
                                          self["mediaText"] = $hs_fromUtf8(val);
                                        });
var webkit_dom_media_list_get_media_text;
webkit_dom_media_list_get_media_text = (function(self)
                                        {
                                          return $hs_toUtf8(self["mediaText"]);
                                        });
var webkit_dom_media_list_get_length;
webkit_dom_media_list_get_length = (function(self)
                                    {
                                      return $hs_int(self["length"]);
                                    });
// Graphics.UI.Gtk.WebKit.DOM.Window
webkit_dom_location_get_type = (function()
                                {
                                  return Location;
                                });
var webkit_dom_location_get_origin;
webkit_dom_location_get_origin = (function(self)
                                  {
                                    return $hs_toUtf8(self["origin"]);
                                  });
// Graphics.UI.Gtk.WebKit.DOM.Events
webkit_dom_keyboard_event_get_type = (function()
                                      {
                                        return KeyboardEvent;
                                      });
var webkit_dom_keyboard_event_get_modifier_state;
webkit_dom_keyboard_event_get_modifier_state = (function(self,
                                                         keyIdentifierArg)
                                                {
                                                  return $hs_int((self["getModifierState"]($hs_fromUtf8(keyIdentifierArg)) ? 1 : 0));
                                                });
var webkit_dom_keyboard_event_init_keyboard_event;
webkit_dom_keyboard_event_init_keyboard_event = (function(self,
                                                          type, canBubble, cancelable, view,
                                                          keyIdentifier, keyLocation, ctrlKey,
                                                          altKey, shiftKey, metaKey, altGraphKey)
                                                 {
                                                   return self["initKeyboardEvent"]($hs_fromUtf8(type),
                                                                                    ($hs_intToNumber(canBubble)
                                                                                     !=
                                                                                     0),
                                                                                    ($hs_intToNumber(cancelable)
                                                                                     !=
                                                                                     0),
                                                                                    view,
                                                                                    $hs_fromUtf8(keyIdentifier),
                                                                                    $hs_intToNumber(keyLocation),
                                                                                    ($hs_intToNumber(ctrlKey)
                                                                                     !=
                                                                                     0),
                                                                                    ($hs_intToNumber(altKey)
                                                                                     !=
                                                                                     0),
                                                                                    ($hs_intToNumber(shiftKey)
                                                                                     !=
                                                                                     0),
                                                                                    ($hs_intToNumber(metaKey)
                                                                                     !=
                                                                                     0),
                                                                                    ($hs_intToNumber(altGraphKey)
                                                                                     !=
                                                                                     0));
                                                 });
var webkit_dom_keyboard_event_init_keyboard_event;
webkit_dom_keyboard_event_init_keyboard_event = (function(self,
                                                          type, canBubble, cancelable, view,
                                                          keyIdentifier, keyLocation, ctrlKey,
                                                          altKey, shiftKey, metaKey)
                                                 {
                                                   return self["initKeyboardEvent"]($hs_fromUtf8(type),
                                                                                    ($hs_intToNumber(canBubble)
                                                                                     !=
                                                                                     0),
                                                                                    ($hs_intToNumber(cancelable)
                                                                                     !=
                                                                                     0),
                                                                                    view,
                                                                                    $hs_fromUtf8(keyIdentifier),
                                                                                    $hs_intToNumber(keyLocation),
                                                                                    ($hs_intToNumber(ctrlKey)
                                                                                     !=
                                                                                     0),
                                                                                    ($hs_intToNumber(altKey)
                                                                                     !=
                                                                                     0),
                                                                                    ($hs_intToNumber(shiftKey)
                                                                                     !=
                                                                                     0),
                                                                                    ($hs_intToNumber(metaKey)
                                                                                     !=
                                                                                     0));
                                                 });
var webkit_dom_keyboard_event_get_key_identifier;
webkit_dom_keyboard_event_get_key_identifier = (function(self)
                                                {
                                                  return $hs_toUtf8(self["keyIdentifier"]);
                                                });
var webkit_dom_keyboard_event_get_key_location;
webkit_dom_keyboard_event_get_key_location = (function(self)
                                              {
                                                return $hs_int(self["keyLocation"]);
                                              });
var webkit_dom_keyboard_event_get_ctrl_key;
webkit_dom_keyboard_event_get_ctrl_key = (function(self)
                                          {
                                            return $hs_int((self["ctrlKey"] ? 1 : 0));
                                          });
var webkit_dom_keyboard_event_get_shift_key;
webkit_dom_keyboard_event_get_shift_key = (function(self)
                                           {
                                             return $hs_int((self["shiftKey"] ? 1 : 0));
                                           });
var webkit_dom_keyboard_event_get_alt_key;
webkit_dom_keyboard_event_get_alt_key = (function(self)
                                         {
                                           return $hs_int((self["altKey"] ? 1 : 0));
                                         });
var webkit_dom_keyboard_event_get_meta_key;
webkit_dom_keyboard_event_get_meta_key = (function(self)
                                          {
                                            return $hs_int((self["metaKey"] ? 1 : 0));
                                          });
var webkit_dom_keyboard_event_get_alt_graph_key;
webkit_dom_keyboard_event_get_alt_graph_key = (function(self)
                                               {
                                                 return $hs_int((self["altGraphKey"] ? 1 : 0));
                                               });
var webkit_dom_keyboard_event_get_key_code;
webkit_dom_keyboard_event_get_key_code = (function(self)
                                          {
                                            return $hs_int(self["keyCode"]);
                                          });
var webkit_dom_keyboard_event_get_char_code;
webkit_dom_keyboard_event_get_char_code = (function(self)
                                           {
                                             return $hs_int(self["charCode"]);
                                           });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_htmlu_list_element_get_type = (function()
                                          {
                                            return HTMLUListElement;
                                          });
var webkit_dom_htmlu_list_element_set_compact;
webkit_dom_htmlu_list_element_set_compact = (function(self, val)
                                             {
                                               self["compact"] = ($hs_intToNumber(val) != 0);
                                             });
var webkit_dom_htmlu_list_element_get_compact;
webkit_dom_htmlu_list_element_get_compact = (function(self)
                                             {
                                               return $hs_int((self["compact"] ? 1 : 0));
                                             });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_html_title_element_get_type = (function()
                                          {
                                            return HTMLTitleElement;
                                          });
var webkit_dom_html_title_element_set_text;
webkit_dom_html_title_element_set_text = (function(self, val)
                                          {
                                            self["text"] = $hs_fromUtf8(val);
                                          });
var webkit_dom_html_title_element_get_text;
webkit_dom_html_title_element_get_text = (function(self)
                                          {
                                            return $hs_toUtf8(self["text"]);
                                          });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_html_text_area_element_get_type = (function()
                                              {
                                                return HTMLTextAreaElement;
                                              });
var webkit_dom_html_text_area_element_select;
webkit_dom_html_text_area_element_select = (function(self)
                                            {
                                              return self["select"]();
                                            });
var webkit_dom_html_text_area_element_check_validity;
webkit_dom_html_text_area_element_check_validity = (function(self)
                                                    {
                                                      return $hs_int((self["checkValidity"]() ? 1 : 0));
                                                    });
var webkit_dom_html_text_area_element_set_custom_validity;
webkit_dom_html_text_area_element_set_custom_validity = (function(self,
                                                                  error)
                                                         {
                                                           return self["setCustomValidity"]($hs_fromUtf8(error));
                                                         });
var webkit_dom_html_text_area_element_set_selection_range;
webkit_dom_html_text_area_element_set_selection_range = (function(self,
                                                                  start, end, direction)
                                                         {
                                                           return self["setSelectionRange"]($hs_intToNumber(start),
                                                                                            $hs_intToNumber(end),
                                                                                            $hs_fromUtf8(direction));
                                                         });
var webkit_dom_html_text_area_element_set_default_value;
webkit_dom_html_text_area_element_set_default_value = (function(self,
                                                                val)
                                                       {
                                                         self["defaultValue"] = $hs_fromUtf8(val);
                                                       });
var webkit_dom_html_text_area_element_get_default_value;
webkit_dom_html_text_area_element_get_default_value = (function(self)
                                                       {
                                                         return $hs_toUtf8(self["defaultValue"]);
                                                       });
var webkit_dom_html_text_area_element_get_form;
webkit_dom_html_text_area_element_get_form = (function(self)
                                              {
                                                return self["form"];
                                              });
var webkit_dom_html_text_area_element_get_validity;
webkit_dom_html_text_area_element_get_validity = (function(self)
                                                  {
                                                    return self["validity"];
                                                  });
var webkit_dom_html_text_area_element_set_cols;
webkit_dom_html_text_area_element_set_cols = (function(self, val)
                                              {
                                                self["cols"] = $hs_intToNumber(val);
                                              });
var webkit_dom_html_text_area_element_get_cols;
webkit_dom_html_text_area_element_get_cols = (function(self)
                                              {
                                                return $hs_int(self["cols"]);
                                              });
var webkit_dom_html_text_area_element_set_dir_name;
webkit_dom_html_text_area_element_set_dir_name = (function(self,
                                                           val)
                                                  {
                                                    self["dirName"] = $hs_fromUtf8(val);
                                                  });
var webkit_dom_html_text_area_element_get_dir_name;
webkit_dom_html_text_area_element_get_dir_name = (function(self)
                                                  {
                                                    return $hs_toUtf8(self["dirName"]);
                                                  });
var webkit_dom_html_text_area_element_set_disabled;
webkit_dom_html_text_area_element_set_disabled = (function(self,
                                                           val)
                                                  {
                                                    self["disabled"] = ($hs_intToNumber(val) != 0);
                                                  });
var webkit_dom_html_text_area_element_get_disabled;
webkit_dom_html_text_area_element_get_disabled = (function(self)
                                                  {
                                                    return $hs_int((self["disabled"] ? 1 : 0));
                                                  });
var webkit_dom_html_text_area_element_set_autofocus;
webkit_dom_html_text_area_element_set_autofocus = (function(self,
                                                            val)
                                                   {
                                                     self["autofocus"] = ($hs_intToNumber(val)
                                                                          !=
                                                                          0);
                                                   });
var webkit_dom_html_text_area_element_get_autofocus;
webkit_dom_html_text_area_element_get_autofocus = (function(self)
                                                   {
                                                     return $hs_int((self["autofocus"] ? 1 : 0));
                                                   });
var webkit_dom_html_text_area_element_set_max_length;
webkit_dom_html_text_area_element_set_max_length = (function(self,
                                                             val)
                                                    {
                                                      self["maxLength"] = $hs_intToNumber(val);
                                                    });
var webkit_dom_html_text_area_element_get_max_length;
webkit_dom_html_text_area_element_get_max_length = (function(self)
                                                    {
                                                      return $hs_int(self["maxLength"]);
                                                    });
var webkit_dom_html_text_area_element_set_name;
webkit_dom_html_text_area_element_set_name = (function(self, val)
                                              {
                                                self["name"] = $hs_fromUtf8(val);
                                              });
var webkit_dom_html_text_area_element_get_name;
webkit_dom_html_text_area_element_get_name = (function(self)
                                              {
                                                return $hs_toUtf8(self["name"]);
                                              });
var webkit_dom_html_text_area_element_set_placeholder;
webkit_dom_html_text_area_element_set_placeholder = (function(self,
                                                              val)
                                                     {
                                                       self["placeholder"] = $hs_fromUtf8(val);
                                                     });
var webkit_dom_html_text_area_element_get_placeholder;
webkit_dom_html_text_area_element_get_placeholder = (function(self)
                                                     {
                                                       return $hs_toUtf8(self["placeholder"]);
                                                     });
var webkit_dom_html_text_area_element_set_read_only;
webkit_dom_html_text_area_element_set_read_only = (function(self,
                                                            val)
                                                   {
                                                     self["readOnly"] = ($hs_intToNumber(val) != 0);
                                                   });
var webkit_dom_html_text_area_element_get_read_only;
webkit_dom_html_text_area_element_get_read_only = (function(self)
                                                   {
                                                     return $hs_int((self["readOnly"] ? 1 : 0));
                                                   });
var webkit_dom_html_text_area_element_set_required;
webkit_dom_html_text_area_element_set_required = (function(self,
                                                           val)
                                                  {
                                                    self["required"] = ($hs_intToNumber(val) != 0);
                                                  });
var webkit_dom_html_text_area_element_get_required;
webkit_dom_html_text_area_element_get_required = (function(self)
                                                  {
                                                    return $hs_int((self["required"] ? 1 : 0));
                                                  });
var webkit_dom_html_text_area_element_set_rows;
webkit_dom_html_text_area_element_set_rows = (function(self, val)
                                              {
                                                self["rows"] = $hs_intToNumber(val);
                                              });
var webkit_dom_html_text_area_element_get_rows;
webkit_dom_html_text_area_element_get_rows = (function(self)
                                              {
                                                return $hs_int(self["rows"]);
                                              });
var webkit_dom_html_text_area_element_set_wrap;
webkit_dom_html_text_area_element_set_wrap = (function(self, val)
                                              {
                                                self["wrap"] = $hs_fromUtf8(val);
                                              });
var webkit_dom_html_text_area_element_get_wrap;
webkit_dom_html_text_area_element_get_wrap = (function(self)
                                              {
                                                return $hs_toUtf8(self["wrap"]);
                                              });
var webkit_dom_html_text_area_element_set_value;
webkit_dom_html_text_area_element_set_value = (function(self, val)
                                               {
                                                 self["value"] = $hs_fromUtf8(val);
                                               });
var webkit_dom_html_text_area_element_get_value;
webkit_dom_html_text_area_element_get_value = (function(self)
                                               {
                                                 return $hs_toUtf8(self["value"]);
                                               });
var webkit_dom_html_text_area_element_get_text_length;
webkit_dom_html_text_area_element_get_text_length = (function(self)
                                                     {
                                                       return $hs_int(self["textLength"]);
                                                     });
var webkit_dom_html_text_area_element_get_will_validate;
webkit_dom_html_text_area_element_get_will_validate = (function(self)
                                                       {
                                                         return $hs_int((self["willValidate"] ? 1 : 0));
                                                       });
var webkit_dom_html_text_area_element_get_validation_message;
webkit_dom_html_text_area_element_get_validation_message = (function(self)
                                                            {
                                                              return $hs_toUtf8(self["validationMessage"]);
                                                            });
var webkit_dom_html_text_area_element_set_selection_start;
webkit_dom_html_text_area_element_set_selection_start = (function(self,
                                                                  val)
                                                         {
                                                           self["selectionStart"] = $hs_intToNumber(val);
                                                         });
var webkit_dom_html_text_area_element_get_selection_start;
webkit_dom_html_text_area_element_get_selection_start = (function(self)
                                                         {
                                                           return $hs_int(self["selectionStart"]);
                                                         });
var webkit_dom_html_text_area_element_set_selection_end;
webkit_dom_html_text_area_element_set_selection_end = (function(self,
                                                                val)
                                                       {
                                                         self["selectionEnd"] = $hs_intToNumber(val);
                                                       });
var webkit_dom_html_text_area_element_get_selection_end;
webkit_dom_html_text_area_element_get_selection_end = (function(self)
                                                       {
                                                         return $hs_int(self["selectionEnd"]);
                                                       });
var webkit_dom_html_text_area_element_set_selection_direction;
webkit_dom_html_text_area_element_set_selection_direction = (function(self,
                                                                      val)
                                                             {
                                                               self["selectionDirection"] = $hs_fromUtf8(val);
                                                             });
var webkit_dom_html_text_area_element_get_selection_direction;
webkit_dom_html_text_area_element_get_selection_direction = (function(self)
                                                             {
                                                               return $hs_toUtf8(self["selectionDirection"]);
                                                             });
var webkit_dom_html_text_area_element_get_labels;
webkit_dom_html_text_area_element_get_labels = (function(self)
                                                {
                                                  return self["labels"];
                                                });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_html_table_section_element_get_type = (function()
                                                  {
                                                    return HTMLTableSectionElement;
                                                  });
var webkit_dom_html_table_section_element_insert_row;
webkit_dom_html_table_section_element_insert_row = (function(self,
                                                             index)
                                                    {
                                                      return self["insertRow"]($hs_intToNumber(index));
                                                    });
var webkit_dom_html_table_section_element_delete_row;
webkit_dom_html_table_section_element_delete_row = (function(self,
                                                             index)
                                                    {
                                                      return self["deleteRow"]($hs_intToNumber(index));
                                                    });
var webkit_dom_html_table_section_element_set_align;
webkit_dom_html_table_section_element_set_align = (function(self,
                                                            val)
                                                   {
                                                     self["align"] = $hs_fromUtf8(val);
                                                   });
var webkit_dom_html_table_section_element_get_align;
webkit_dom_html_table_section_element_get_align = (function(self)
                                                   {
                                                     return $hs_toUtf8(self["align"]);
                                                   });
var webkit_dom_html_table_section_element_set_ch;
webkit_dom_html_table_section_element_set_ch = (function(self, val)
                                                {
                                                  self["ch"] = $hs_fromUtf8(val);
                                                });
var webkit_dom_html_table_section_element_get_ch;
webkit_dom_html_table_section_element_get_ch = (function(self)
                                                {
                                                  return $hs_toUtf8(self["ch"]);
                                                });
var webkit_dom_html_table_section_element_set_ch_off;
webkit_dom_html_table_section_element_set_ch_off = (function(self,
                                                             val)
                                                    {
                                                      self["chOff"] = $hs_fromUtf8(val);
                                                    });
var webkit_dom_html_table_section_element_get_ch_off;
webkit_dom_html_table_section_element_get_ch_off = (function(self)
                                                    {
                                                      return $hs_toUtf8(self["chOff"]);
                                                    });
var webkit_dom_html_table_section_element_set_v_align;
webkit_dom_html_table_section_element_set_v_align = (function(self,
                                                              val)
                                                     {
                                                       self["vAlign"] = $hs_fromUtf8(val);
                                                     });
var webkit_dom_html_table_section_element_get_v_align;
webkit_dom_html_table_section_element_get_v_align = (function(self)
                                                     {
                                                       return $hs_toUtf8(self["vAlign"]);
                                                     });
var webkit_dom_html_table_section_element_get_rows;
webkit_dom_html_table_section_element_get_rows = (function(self)
                                                  {
                                                    return self["rows"];
                                                  });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_html_table_row_element_get_type = (function()
                                              {
                                                return HTMLTableRowElement;
                                              });
var webkit_dom_html_table_row_element_insert_cell;
webkit_dom_html_table_row_element_insert_cell = (function(self,
                                                          index)
                                                 {
                                                   return self["insertCell"]($hs_intToNumber(index));
                                                 });
var webkit_dom_html_table_row_element_delete_cell;
webkit_dom_html_table_row_element_delete_cell = (function(self,
                                                          index)
                                                 {
                                                   return self["deleteCell"]($hs_intToNumber(index));
                                                 });
var webkit_dom_html_table_row_element_get_row_index;
webkit_dom_html_table_row_element_get_row_index = (function(self)
                                                   {
                                                     return $hs_int(self["rowIndex"]);
                                                   });
var webkit_dom_html_table_row_element_get_section_row_index;
webkit_dom_html_table_row_element_get_section_row_index = (function(self)
                                                           {
                                                             return $hs_int(self["sectionRowIndex"]);
                                                           });
var webkit_dom_html_table_row_element_get_cells;
webkit_dom_html_table_row_element_get_cells = (function(self)
                                               {
                                                 return self["cells"];
                                               });
var webkit_dom_html_table_row_element_set_align;
webkit_dom_html_table_row_element_set_align = (function(self, val)
                                               {
                                                 self["align"] = $hs_fromUtf8(val);
                                               });
var webkit_dom_html_table_row_element_get_align;
webkit_dom_html_table_row_element_get_align = (function(self)
                                               {
                                                 return $hs_toUtf8(self["align"]);
                                               });
var webkit_dom_html_table_row_element_set_bg_color;
webkit_dom_html_table_row_element_set_bg_color = (function(self,
                                                           val)
                                                  {
                                                    self["bgColor"] = $hs_fromUtf8(val);
                                                  });
var webkit_dom_html_table_row_element_get_bg_color;
webkit_dom_html_table_row_element_get_bg_color = (function(self)
                                                  {
                                                    return $hs_toUtf8(self["bgColor"]);
                                                  });
var webkit_dom_html_table_row_element_set_ch;
webkit_dom_html_table_row_element_set_ch = (function(self, val)
                                            {
                                              self["ch"] = $hs_fromUtf8(val);
                                            });
var webkit_dom_html_table_row_element_get_ch;
webkit_dom_html_table_row_element_get_ch = (function(self)
                                            {
                                              return $hs_toUtf8(self["ch"]);
                                            });
var webkit_dom_html_table_row_element_set_ch_off;
webkit_dom_html_table_row_element_set_ch_off = (function(self, val)
                                                {
                                                  self["chOff"] = $hs_fromUtf8(val);
                                                });
var webkit_dom_html_table_row_element_get_ch_off;
webkit_dom_html_table_row_element_get_ch_off = (function(self)
                                                {
                                                  return $hs_toUtf8(self["chOff"]);
                                                });
var webkit_dom_html_table_row_element_set_v_align;
webkit_dom_html_table_row_element_set_v_align = (function(self,
                                                          val)
                                                 {
                                                   self["vAlign"] = $hs_fromUtf8(val);
                                                 });
var webkit_dom_html_table_row_element_get_v_align;
webkit_dom_html_table_row_element_get_v_align = (function(self)
                                                 {
                                                   return $hs_toUtf8(self["vAlign"]);
                                                 });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_html_table_element_get_type = (function()
                                          {
                                            return HTMLTableElement;
                                          });
var webkit_dom_html_table_element_create_t_head;
webkit_dom_html_table_element_create_t_head = (function(self)
                                               {
                                                 return self["createTHead"]();
                                               });
var webkit_dom_html_table_element_delete_t_head;
webkit_dom_html_table_element_delete_t_head = (function(self)
                                               {
                                                 return self["deleteTHead"]();
                                               });
var webkit_dom_html_table_element_create_t_foot;
webkit_dom_html_table_element_create_t_foot = (function(self)
                                               {
                                                 return self["createTFoot"]();
                                               });
var webkit_dom_html_table_element_delete_t_foot;
webkit_dom_html_table_element_delete_t_foot = (function(self)
                                               {
                                                 return self["deleteTFoot"]();
                                               });
var webkit_dom_html_table_element_create_caption;
webkit_dom_html_table_element_create_caption = (function(self)
                                                {
                                                  return self["createCaption"]();
                                                });
var webkit_dom_html_table_element_delete_caption;
webkit_dom_html_table_element_delete_caption = (function(self)
                                                {
                                                  return self["deleteCaption"]();
                                                });
var webkit_dom_html_table_element_insert_row;
webkit_dom_html_table_element_insert_row = (function(self, index)
                                            {
                                              return self["insertRow"]($hs_intToNumber(index));
                                            });
var webkit_dom_html_table_element_delete_row;
webkit_dom_html_table_element_delete_row = (function(self, index)
                                            {
                                              return self["deleteRow"]($hs_intToNumber(index));
                                            });
var webkit_dom_html_table_element_set_caption;
webkit_dom_html_table_element_set_caption = (function(self, val)
                                             {
                                               self["caption"] = val;
                                             });
var webkit_dom_html_table_element_get_caption;
webkit_dom_html_table_element_get_caption = (function(self)
                                             {
                                               return self["caption"];
                                             });
var webkit_dom_html_table_element_set_t_head;
webkit_dom_html_table_element_set_t_head = (function(self, val)
                                            {
                                              self["tHead"] = val;
                                            });
var webkit_dom_html_table_element_get_t_head;
webkit_dom_html_table_element_get_t_head = (function(self)
                                            {
                                              return self["tHead"];
                                            });
var webkit_dom_html_table_element_set_t_foot;
webkit_dom_html_table_element_set_t_foot = (function(self, val)
                                            {
                                              self["tFoot"] = val;
                                            });
var webkit_dom_html_table_element_get_t_foot;
webkit_dom_html_table_element_get_t_foot = (function(self)
                                            {
                                              return self["tFoot"];
                                            });
var webkit_dom_html_table_element_get_rows;
webkit_dom_html_table_element_get_rows = (function(self)
                                          {
                                            return self["rows"];
                                          });
var webkit_dom_html_table_element_get_t_bodies;
webkit_dom_html_table_element_get_t_bodies = (function(self)
                                              {
                                                return self["tBodies"];
                                              });
var webkit_dom_html_table_element_set_align;
webkit_dom_html_table_element_set_align = (function(self, val)
                                           {
                                             self["align"] = $hs_fromUtf8(val);
                                           });
var webkit_dom_html_table_element_get_align;
webkit_dom_html_table_element_get_align = (function(self)
                                           {
                                             return $hs_toUtf8(self["align"]);
                                           });
var webkit_dom_html_table_element_set_bg_color;
webkit_dom_html_table_element_set_bg_color = (function(self, val)
                                              {
                                                self["bgColor"] = $hs_fromUtf8(val);
                                              });
var webkit_dom_html_table_element_get_bg_color;
webkit_dom_html_table_element_get_bg_color = (function(self)
                                              {
                                                return $hs_toUtf8(self["bgColor"]);
                                              });
var webkit_dom_html_table_element_set_border;
webkit_dom_html_table_element_set_border = (function(self, val)
                                            {
                                              self["border"] = $hs_fromUtf8(val);
                                            });
var webkit_dom_html_table_element_get_border;
webkit_dom_html_table_element_get_border = (function(self)
                                            {
                                              return $hs_toUtf8(self["border"]);
                                            });
var webkit_dom_html_table_element_set_cell_padding;
webkit_dom_html_table_element_set_cell_padding = (function(self,
                                                           val)
                                                  {
                                                    self["cellPadding"] = $hs_fromUtf8(val);
                                                  });
var webkit_dom_html_table_element_get_cell_padding;
webkit_dom_html_table_element_get_cell_padding = (function(self)
                                                  {
                                                    return $hs_toUtf8(self["cellPadding"]);
                                                  });
var webkit_dom_html_table_element_set_cell_spacing;
webkit_dom_html_table_element_set_cell_spacing = (function(self,
                                                           val)
                                                  {
                                                    self["cellSpacing"] = $hs_fromUtf8(val);
                                                  });
var webkit_dom_html_table_element_get_cell_spacing;
webkit_dom_html_table_element_get_cell_spacing = (function(self)
                                                  {
                                                    return $hs_toUtf8(self["cellSpacing"]);
                                                  });
var webkit_dom_html_table_element_set_frame;
webkit_dom_html_table_element_set_frame = (function(self, val)
                                           {
                                             self["frame"] = $hs_fromUtf8(val);
                                           });
var webkit_dom_html_table_element_get_frame;
webkit_dom_html_table_element_get_frame = (function(self)
                                           {
                                             return $hs_toUtf8(self["frame"]);
                                           });
var webkit_dom_html_table_element_set_rules;
webkit_dom_html_table_element_set_rules = (function(self, val)
                                           {
                                             self["rules"] = $hs_fromUtf8(val);
                                           });
var webkit_dom_html_table_element_get_rules;
webkit_dom_html_table_element_get_rules = (function(self)
                                           {
                                             return $hs_toUtf8(self["rules"]);
                                           });
var webkit_dom_html_table_element_set_summary;
webkit_dom_html_table_element_set_summary = (function(self, val)
                                             {
                                               self["summary"] = $hs_fromUtf8(val);
                                             });
var webkit_dom_html_table_element_get_summary;
webkit_dom_html_table_element_get_summary = (function(self)
                                             {
                                               return $hs_toUtf8(self["summary"]);
                                             });
var webkit_dom_html_table_element_set_width;
webkit_dom_html_table_element_set_width = (function(self, val)
                                           {
                                             self["width"] = $hs_fromUtf8(val);
                                           });
var webkit_dom_html_table_element_get_width;
webkit_dom_html_table_element_get_width = (function(self)
                                           {
                                             return $hs_toUtf8(self["width"]);
                                           });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_html_table_col_element_get_type = (function()
                                              {
                                                return HTMLTableColElement;
                                              });
var webkit_dom_html_table_col_element_set_align;
webkit_dom_html_table_col_element_set_align = (function(self, val)
                                               {
                                                 self["align"] = $hs_fromUtf8(val);
                                               });
var webkit_dom_html_table_col_element_get_align;
webkit_dom_html_table_col_element_get_align = (function(self)
                                               {
                                                 return $hs_toUtf8(self["align"]);
                                               });
var webkit_dom_html_table_col_element_set_ch;
webkit_dom_html_table_col_element_set_ch = (function(self, val)
                                            {
                                              self["ch"] = $hs_fromUtf8(val);
                                            });
var webkit_dom_html_table_col_element_get_ch;
webkit_dom_html_table_col_element_get_ch = (function(self)
                                            {
                                              return $hs_toUtf8(self["ch"]);
                                            });
var webkit_dom_html_table_col_element_set_ch_off;
webkit_dom_html_table_col_element_set_ch_off = (function(self, val)
                                                {
                                                  self["chOff"] = $hs_fromUtf8(val);
                                                });
var webkit_dom_html_table_col_element_get_ch_off;
webkit_dom_html_table_col_element_get_ch_off = (function(self)
                                                {
                                                  return $hs_toUtf8(self["chOff"]);
                                                });
var webkit_dom_html_table_col_element_set_span;
webkit_dom_html_table_col_element_set_span = (function(self, val)
                                              {
                                                self["span"] = $hs_intToNumber(val);
                                              });
var webkit_dom_html_table_col_element_get_span;
webkit_dom_html_table_col_element_get_span = (function(self)
                                              {
                                                return $hs_int(self["span"]);
                                              });
var webkit_dom_html_table_col_element_set_v_align;
webkit_dom_html_table_col_element_set_v_align = (function(self,
                                                          val)
                                                 {
                                                   self["vAlign"] = $hs_fromUtf8(val);
                                                 });
var webkit_dom_html_table_col_element_get_v_align;
webkit_dom_html_table_col_element_get_v_align = (function(self)
                                                 {
                                                   return $hs_toUtf8(self["vAlign"]);
                                                 });
var webkit_dom_html_table_col_element_set_width;
webkit_dom_html_table_col_element_set_width = (function(self, val)
                                               {
                                                 self["width"] = $hs_fromUtf8(val);
                                               });
var webkit_dom_html_table_col_element_get_width;
webkit_dom_html_table_col_element_get_width = (function(self)
                                               {
                                                 return $hs_toUtf8(self["width"]);
                                               });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_html_table_cell_element_get_type = (function()
                                               {
                                                 return HTMLTableCellElement;
                                               });
var webkit_dom_html_table_cell_element_get_cell_index;
webkit_dom_html_table_cell_element_get_cell_index = (function(self)
                                                     {
                                                       return $hs_int(self["cellIndex"]);
                                                     });
var webkit_dom_html_table_cell_element_set_abbr;
webkit_dom_html_table_cell_element_set_abbr = (function(self, val)
                                               {
                                                 self["abbr"] = $hs_fromUtf8(val);
                                               });
var webkit_dom_html_table_cell_element_get_abbr;
webkit_dom_html_table_cell_element_get_abbr = (function(self)
                                               {
                                                 return $hs_toUtf8(self["abbr"]);
                                               });
var webkit_dom_html_table_cell_element_set_align;
webkit_dom_html_table_cell_element_set_align = (function(self, val)
                                                {
                                                  self["align"] = $hs_fromUtf8(val);
                                                });
var webkit_dom_html_table_cell_element_get_align;
webkit_dom_html_table_cell_element_get_align = (function(self)
                                                {
                                                  return $hs_toUtf8(self["align"]);
                                                });
var webkit_dom_html_table_cell_element_set_axis;
webkit_dom_html_table_cell_element_set_axis = (function(self, val)
                                               {
                                                 self["axis"] = $hs_fromUtf8(val);
                                               });
var webkit_dom_html_table_cell_element_get_axis;
webkit_dom_html_table_cell_element_get_axis = (function(self)
                                               {
                                                 return $hs_toUtf8(self["axis"]);
                                               });
var webkit_dom_html_table_cell_element_set_bg_color;
webkit_dom_html_table_cell_element_set_bg_color = (function(self,
                                                            val)
                                                   {
                                                     self["bgColor"] = $hs_fromUtf8(val);
                                                   });
var webkit_dom_html_table_cell_element_get_bg_color;
webkit_dom_html_table_cell_element_get_bg_color = (function(self)
                                                   {
                                                     return $hs_toUtf8(self["bgColor"]);
                                                   });
var webkit_dom_html_table_cell_element_set_ch;
webkit_dom_html_table_cell_element_set_ch = (function(self, val)
                                             {
                                               self["ch"] = $hs_fromUtf8(val);
                                             });
var webkit_dom_html_table_cell_element_get_ch;
webkit_dom_html_table_cell_element_get_ch = (function(self)
                                             {
                                               return $hs_toUtf8(self["ch"]);
                                             });
var webkit_dom_html_table_cell_element_set_ch_off;
webkit_dom_html_table_cell_element_set_ch_off = (function(self,
                                                          val)
                                                 {
                                                   self["chOff"] = $hs_fromUtf8(val);
                                                 });
var webkit_dom_html_table_cell_element_get_ch_off;
webkit_dom_html_table_cell_element_get_ch_off = (function(self)
                                                 {
                                                   return $hs_toUtf8(self["chOff"]);
                                                 });
var webkit_dom_html_table_cell_element_set_col_span;
webkit_dom_html_table_cell_element_set_col_span = (function(self,
                                                            val)
                                                   {
                                                     self["colSpan"] = $hs_intToNumber(val);
                                                   });
var webkit_dom_html_table_cell_element_get_col_span;
webkit_dom_html_table_cell_element_get_col_span = (function(self)
                                                   {
                                                     return $hs_int(self["colSpan"]);
                                                   });
var webkit_dom_html_table_cell_element_set_headers;
webkit_dom_html_table_cell_element_set_headers = (function(self,
                                                           val)
                                                  {
                                                    self["headers"] = $hs_fromUtf8(val);
                                                  });
var webkit_dom_html_table_cell_element_get_headers;
webkit_dom_html_table_cell_element_get_headers = (function(self)
                                                  {
                                                    return $hs_toUtf8(self["headers"]);
                                                  });
var webkit_dom_html_table_cell_element_set_height;
webkit_dom_html_table_cell_element_set_height = (function(self,
                                                          val)
                                                 {
                                                   self["height"] = $hs_fromUtf8(val);
                                                 });
var webkit_dom_html_table_cell_element_get_height;
webkit_dom_html_table_cell_element_get_height = (function(self)
                                                 {
                                                   return $hs_toUtf8(self["height"]);
                                                 });
var webkit_dom_html_table_cell_element_set_no_wrap;
webkit_dom_html_table_cell_element_set_no_wrap = (function(self,
                                                           val)
                                                  {
                                                    self["noWrap"] = ($hs_intToNumber(val) != 0);
                                                  });
var webkit_dom_html_table_cell_element_get_no_wrap;
webkit_dom_html_table_cell_element_get_no_wrap = (function(self)
                                                  {
                                                    return $hs_int((self["noWrap"] ? 1 : 0));
                                                  });
var webkit_dom_html_table_cell_element_set_row_span;
webkit_dom_html_table_cell_element_set_row_span = (function(self,
                                                            val)
                                                   {
                                                     self["rowSpan"] = $hs_intToNumber(val);
                                                   });
var webkit_dom_html_table_cell_element_get_row_span;
webkit_dom_html_table_cell_element_get_row_span = (function(self)
                                                   {
                                                     return $hs_int(self["rowSpan"]);
                                                   });
var webkit_dom_html_table_cell_element_set_scope;
webkit_dom_html_table_cell_element_set_scope = (function(self, val)
                                                {
                                                  self["scope"] = $hs_fromUtf8(val);
                                                });
var webkit_dom_html_table_cell_element_get_scope;
webkit_dom_html_table_cell_element_get_scope = (function(self)
                                                {
                                                  return $hs_toUtf8(self["scope"]);
                                                });
var webkit_dom_html_table_cell_element_set_v_align;
webkit_dom_html_table_cell_element_set_v_align = (function(self,
                                                           val)
                                                  {
                                                    self["vAlign"] = $hs_fromUtf8(val);
                                                  });
var webkit_dom_html_table_cell_element_get_v_align;
webkit_dom_html_table_cell_element_get_v_align = (function(self)
                                                  {
                                                    return $hs_toUtf8(self["vAlign"]);
                                                  });
var webkit_dom_html_table_cell_element_set_width;
webkit_dom_html_table_cell_element_set_width = (function(self, val)
                                                {
                                                  self["width"] = $hs_fromUtf8(val);
                                                });
var webkit_dom_html_table_cell_element_get_width;
webkit_dom_html_table_cell_element_get_width = (function(self)
                                                {
                                                  return $hs_toUtf8(self["width"]);
                                                });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_html_table_caption_element_get_type = (function()
                                                  {
                                                    return HTMLTableCaptionElement;
                                                  });
var webkit_dom_html_table_caption_element_set_align;
webkit_dom_html_table_caption_element_set_align = (function(self,
                                                            val)
                                                   {
                                                     self["align"] = $hs_fromUtf8(val);
                                                   });
var webkit_dom_html_table_caption_element_get_align;
webkit_dom_html_table_caption_element_get_align = (function(self)
                                                   {
                                                     return $hs_toUtf8(self["align"]);
                                                   });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_html_style_element_get_type = (function()
                                          {
                                            return HTMLStyleElement;
                                          });
var webkit_dom_html_style_element_set_disabled;
webkit_dom_html_style_element_set_disabled = (function(self, val)
                                              {
                                                self["disabled"] = ($hs_intToNumber(val) != 0);
                                              });
var webkit_dom_html_style_element_get_disabled;
webkit_dom_html_style_element_get_disabled = (function(self)
                                              {
                                                return $hs_int((self["disabled"] ? 1 : 0));
                                              });
var webkit_dom_html_style_element_set_scoped;
webkit_dom_html_style_element_set_scoped = (function(self, val)
                                            {
                                              self["scoped"] = ($hs_intToNumber(val) != 0);
                                            });
var webkit_dom_html_style_element_get_scoped;
webkit_dom_html_style_element_get_scoped = (function(self)
                                            {
                                              return $hs_int((self["scoped"] ? 1 : 0));
                                            });
var webkit_dom_html_style_element_set_media;
webkit_dom_html_style_element_set_media = (function(self, val)
                                           {
                                             self["media"] = $hs_fromUtf8(val);
                                           });
var webkit_dom_html_style_element_get_media;
webkit_dom_html_style_element_get_media = (function(self)
                                           {
                                             return $hs_toUtf8(self["media"]);
                                           });
var webkit_dom_html_style_element_get_sheet;
webkit_dom_html_style_element_get_sheet = (function(self)
                                           {
                                             return self["sheet"];
                                           });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_html_select_element_get_type = (function()
                                           {
                                             return HTMLSelectElement;
                                           });
var webkit_dom_html_select_element_check_validity;
webkit_dom_html_select_element_check_validity = (function(self)
                                                 {
                                                   return $hs_int((self["checkValidity"]() ? 1 : 0));
                                                 });
var webkit_dom_html_select_element_set_custom_validity;
webkit_dom_html_select_element_set_custom_validity = (function(self,
                                                               error)
                                                      {
                                                        return self["setCustomValidity"]($hs_fromUtf8(error));
                                                      });
var webkit_dom_html_select_element_add;
webkit_dom_html_select_element_add = (function(self, element,
                                               before)
                                      {
                                        return self["add"](element, before);
                                      });
var webkit_dom_html_select_element_remove;
webkit_dom_html_select_element_remove = (function(self, index)
                                         {
                                           return self["remove"]($hs_intToNumber(index));
                                         });
var webkit_dom_html_select_element_item;
webkit_dom_html_select_element_item = (function(self, index)
                                       {
                                         return self["item"]($hs_intToNumber(index));
                                       });
var webkit_dom_html_select_element_named_item;
webkit_dom_html_select_element_named_item = (function(self, name)
                                             {
                                               return self["namedItem"]($hs_fromUtf8(name));
                                             });
var webkit_dom_html_select_element_set_selected_index;
webkit_dom_html_select_element_set_selected_index = (function(self,
                                                              val)
                                                     {
                                                       self["selectedIndex"] = $hs_intToNumber(val);
                                                     });
var webkit_dom_html_select_element_get_selected_index;
webkit_dom_html_select_element_get_selected_index = (function(self)
                                                     {
                                                       return $hs_int(self["selectedIndex"]);
                                                     });
var webkit_dom_html_select_element_set_value;
webkit_dom_html_select_element_set_value = (function(self, val)
                                            {
                                              self["value"] = $hs_fromUtf8(val);
                                            });
var webkit_dom_html_select_element_get_value;
webkit_dom_html_select_element_get_value = (function(self)
                                            {
                                              return $hs_toUtf8(self["value"]);
                                            });
var webkit_dom_html_select_element_set_length;
webkit_dom_html_select_element_set_length = (function(self, val)
                                             {
                                               self["length"] = $hs_intToNumber(val);
                                             });
var webkit_dom_html_select_element_get_length;
webkit_dom_html_select_element_get_length = (function(self)
                                             {
                                               return $hs_int(self["length"]);
                                             });
var webkit_dom_html_select_element_get_form;
webkit_dom_html_select_element_get_form = (function(self)
                                           {
                                             return self["form"];
                                           });
var webkit_dom_html_select_element_get_validity;
webkit_dom_html_select_element_get_validity = (function(self)
                                               {
                                                 return self["validity"];
                                               });
var webkit_dom_html_select_element_get_will_validate;
webkit_dom_html_select_element_get_will_validate = (function(self)
                                                    {
                                                      return $hs_int((self["willValidate"] ? 1 : 0));
                                                    });
var webkit_dom_html_select_element_get_validation_message;
webkit_dom_html_select_element_get_validation_message = (function(self)
                                                         {
                                                           return $hs_toUtf8(self["validationMessage"]);
                                                         });
var webkit_dom_html_select_element_get_options;
webkit_dom_html_select_element_get_options = (function(self)
                                              {
                                                return self["options"];
                                              });
var webkit_dom_html_select_element_set_disabled;
webkit_dom_html_select_element_set_disabled = (function(self, val)
                                               {
                                                 self["disabled"] = ($hs_intToNumber(val) != 0);
                                               });
var webkit_dom_html_select_element_get_disabled;
webkit_dom_html_select_element_get_disabled = (function(self)
                                               {
                                                 return $hs_int((self["disabled"] ? 1 : 0));
                                               });
var webkit_dom_html_select_element_set_autofocus;
webkit_dom_html_select_element_set_autofocus = (function(self, val)
                                                {
                                                  self["autofocus"] = ($hs_intToNumber(val) != 0);
                                                });
var webkit_dom_html_select_element_get_autofocus;
webkit_dom_html_select_element_get_autofocus = (function(self)
                                                {
                                                  return $hs_int((self["autofocus"] ? 1 : 0));
                                                });
var webkit_dom_html_select_element_set_multiple;
webkit_dom_html_select_element_set_multiple = (function(self, val)
                                               {
                                                 self["multiple"] = ($hs_intToNumber(val) != 0);
                                               });
var webkit_dom_html_select_element_get_multiple;
webkit_dom_html_select_element_get_multiple = (function(self)
                                               {
                                                 return $hs_int((self["multiple"] ? 1 : 0));
                                               });
var webkit_dom_html_select_element_set_name;
webkit_dom_html_select_element_set_name = (function(self, val)
                                           {
                                             self["name"] = $hs_fromUtf8(val);
                                           });
var webkit_dom_html_select_element_get_name;
webkit_dom_html_select_element_get_name = (function(self)
                                           {
                                             return $hs_toUtf8(self["name"]);
                                           });
var webkit_dom_html_select_element_set_required;
webkit_dom_html_select_element_set_required = (function(self, val)
                                               {
                                                 self["required"] = ($hs_intToNumber(val) != 0);
                                               });
var webkit_dom_html_select_element_get_required;
webkit_dom_html_select_element_get_required = (function(self)
                                               {
                                                 return $hs_int((self["required"] ? 1 : 0));
                                               });
var webkit_dom_html_select_element_set_size;
webkit_dom_html_select_element_set_size = (function(self, val)
                                           {
                                             self["size"] = $hs_intToNumber(val);
                                           });
var webkit_dom_html_select_element_get_size;
webkit_dom_html_select_element_get_size = (function(self)
                                           {
                                             return $hs_int(self["size"]);
                                           });
var webkit_dom_html_select_element_get_labels;
webkit_dom_html_select_element_get_labels = (function(self)
                                             {
                                               return self["labels"];
                                             });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_html_script_element_get_type = (function()
                                           {
                                             return HTMLScriptElement;
                                           });
var webkit_dom_html_script_element_set_text;
webkit_dom_html_script_element_set_text = (function(self, val)
                                           {
                                             self["text"] = $hs_fromUtf8(val);
                                           });
var webkit_dom_html_script_element_get_text;
webkit_dom_html_script_element_get_text = (function(self)
                                           {
                                             return $hs_toUtf8(self["text"]);
                                           });
var webkit_dom_html_script_element_set_html_for;
webkit_dom_html_script_element_set_html_for = (function(self, val)
                                               {
                                                 self["htmlFor"] = $hs_fromUtf8(val);
                                               });
var webkit_dom_html_script_element_get_html_for;
webkit_dom_html_script_element_get_html_for = (function(self)
                                               {
                                                 return $hs_toUtf8(self["htmlFor"]);
                                               });
var webkit_dom_html_script_element_set_event;
webkit_dom_html_script_element_set_event = (function(self, val)
                                            {
                                              self["event"] = $hs_fromUtf8(val);
                                            });
var webkit_dom_html_script_element_get_event;
webkit_dom_html_script_element_get_event = (function(self)
                                            {
                                              return $hs_toUtf8(self["event"]);
                                            });
var webkit_dom_html_script_element_set_charset;
webkit_dom_html_script_element_set_charset = (function(self, val)
                                              {
                                                self["charset"] = $hs_fromUtf8(val);
                                              });
var webkit_dom_html_script_element_get_charset;
webkit_dom_html_script_element_get_charset = (function(self)
                                              {
                                                return $hs_toUtf8(self["charset"]);
                                              });
var webkit_dom_html_script_element_set_async;
webkit_dom_html_script_element_set_async = (function(self, val)
                                            {
                                              self["async"] = ($hs_intToNumber(val) != 0);
                                            });
var webkit_dom_html_script_element_get_async;
webkit_dom_html_script_element_get_async = (function(self)
                                            {
                                              return $hs_int((self["async"] ? 1 : 0));
                                            });
var webkit_dom_html_script_element_set_defer;
webkit_dom_html_script_element_set_defer = (function(self, val)
                                            {
                                              self["defer"] = ($hs_intToNumber(val) != 0);
                                            });
var webkit_dom_html_script_element_get_defer;
webkit_dom_html_script_element_get_defer = (function(self)
                                            {
                                              return $hs_int((self["defer"] ? 1 : 0));
                                            });
var webkit_dom_html_script_element_set_src;
webkit_dom_html_script_element_set_src = (function(self, val)
                                          {
                                            self["src"] = $hs_fromUtf8(val);
                                          });
var webkit_dom_html_script_element_get_src;
webkit_dom_html_script_element_get_src = (function(self)
                                          {
                                            return $hs_toUtf8(self["src"]);
                                          });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_html_quote_element_get_type = (function()
                                          {
                                            return HTMLQuoteElement;
                                          });
var webkit_dom_html_quote_element_set_cite;
webkit_dom_html_quote_element_set_cite = (function(self, val)
                                          {
                                            self["cite"] = $hs_fromUtf8(val);
                                          });
var webkit_dom_html_quote_element_get_cite;
webkit_dom_html_quote_element_get_cite = (function(self)
                                          {
                                            return $hs_toUtf8(self["cite"]);
                                          });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_html_pre_element_get_type = (function()
                                        {
                                          return HTMLPreElement;
                                        });
var webkit_dom_html_pre_element_set_width;
webkit_dom_html_pre_element_set_width = (function(self, val)
                                         {
                                           self["width"] = $hs_intToNumber(val);
                                         });
var webkit_dom_html_pre_element_get_width;
webkit_dom_html_pre_element_get_width = (function(self)
                                         {
                                           return $hs_int(self["width"]);
                                         });
var webkit_dom_html_pre_element_set_wrap;
webkit_dom_html_pre_element_set_wrap = (function(self, val)
                                        {
                                          self["wrap"] = ($hs_intToNumber(val) != 0);
                                        });
var webkit_dom_html_pre_element_get_wrap;
webkit_dom_html_pre_element_get_wrap = (function(self)
                                        {
                                          return $hs_int((self["wrap"] ? 1 : 0));
                                        });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_html_param_element_get_type = (function()
                                          {
                                            return HTMLParamElement;
                                          });
var webkit_dom_html_param_element_set_name;
webkit_dom_html_param_element_set_name = (function(self, val)
                                          {
                                            self["name"] = $hs_fromUtf8(val);
                                          });
var webkit_dom_html_param_element_get_name;
webkit_dom_html_param_element_get_name = (function(self)
                                          {
                                            return $hs_toUtf8(self["name"]);
                                          });
var webkit_dom_html_param_element_set_value;
webkit_dom_html_param_element_set_value = (function(self, val)
                                           {
                                             self["value"] = $hs_fromUtf8(val);
                                           });
var webkit_dom_html_param_element_get_value;
webkit_dom_html_param_element_get_value = (function(self)
                                           {
                                             return $hs_toUtf8(self["value"]);
                                           });
var webkit_dom_html_param_element_set_value_type;
webkit_dom_html_param_element_set_value_type = (function(self, val)
                                                {
                                                  self["valueType"] = $hs_fromUtf8(val);
                                                });
var webkit_dom_html_param_element_get_value_type;
webkit_dom_html_param_element_get_value_type = (function(self)
                                                {
                                                  return $hs_toUtf8(self["valueType"]);
                                                });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_html_paragraph_element_get_type = (function()
                                              {
                                                return HTMLParagraphElement;
                                              });
var webkit_dom_html_paragraph_element_set_align;
webkit_dom_html_paragraph_element_set_align = (function(self, val)
                                               {
                                                 self["align"] = $hs_fromUtf8(val);
                                               });
var webkit_dom_html_paragraph_element_get_align;
webkit_dom_html_paragraph_element_get_align = (function(self)
                                               {
                                                 return $hs_toUtf8(self["align"]);
                                               });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_html_options_collection_get_type = (function()
                                               {
                                                 return HTMLOptionsCollection;
                                               });
var webkit_dom_html_options_collection_set_selected_index;
webkit_dom_html_options_collection_set_selected_index = (function(self,
                                                                  val)
                                                         {
                                                           self["selectedIndex"] = $hs_intToNumber(val);
                                                         });
var webkit_dom_html_options_collection_get_selected_index;
webkit_dom_html_options_collection_get_selected_index = (function(self)
                                                         {
                                                           return $hs_int(self["selectedIndex"]);
                                                         });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_html_option_element_get_type = (function()
                                           {
                                             return HTMLOptionElement;
                                           });
var webkit_dom_html_option_element_get_form;
webkit_dom_html_option_element_get_form = (function(self)
                                           {
                                             return self["form"];
                                           });
var webkit_dom_html_option_element_set_default_selected;
webkit_dom_html_option_element_set_default_selected = (function(self,
                                                                val)
                                                       {
                                                         self["defaultSelected"] = ($hs_intToNumber(val)
                                                                                    !=
                                                                                    0);
                                                       });
var webkit_dom_html_option_element_get_default_selected;
webkit_dom_html_option_element_get_default_selected = (function(self)
                                                       {
                                                         return $hs_int((self["defaultSelected"] ? 1 : 0));
                                                       });
var webkit_dom_html_option_element_get_text;
webkit_dom_html_option_element_get_text = (function(self)
                                           {
                                             return $hs_toUtf8(self["text"]);
                                           });
var webkit_dom_html_option_element_get_index;
webkit_dom_html_option_element_get_index = (function(self)
                                            {
                                              return $hs_int(self["index"]);
                                            });
var webkit_dom_html_option_element_set_disabled;
webkit_dom_html_option_element_set_disabled = (function(self, val)
                                               {
                                                 self["disabled"] = ($hs_intToNumber(val) != 0);
                                               });
var webkit_dom_html_option_element_get_disabled;
webkit_dom_html_option_element_get_disabled = (function(self)
                                               {
                                                 return $hs_int((self["disabled"] ? 1 : 0));
                                               });
var webkit_dom_html_option_element_set_label;
webkit_dom_html_option_element_set_label = (function(self, val)
                                            {
                                              self["label"] = $hs_fromUtf8(val);
                                            });
var webkit_dom_html_option_element_get_label;
webkit_dom_html_option_element_get_label = (function(self)
                                            {
                                              return $hs_toUtf8(self["label"]);
                                            });
var webkit_dom_html_option_element_set_selected;
webkit_dom_html_option_element_set_selected = (function(self, val)
                                               {
                                                 self["selected"] = ($hs_intToNumber(val) != 0);
                                               });
var webkit_dom_html_option_element_get_selected;
webkit_dom_html_option_element_get_selected = (function(self)
                                               {
                                                 return $hs_int((self["selected"] ? 1 : 0));
                                               });
var webkit_dom_html_option_element_set_value;
webkit_dom_html_option_element_set_value = (function(self, val)
                                            {
                                              self["value"] = $hs_fromUtf8(val);
                                            });
var webkit_dom_html_option_element_get_value;
webkit_dom_html_option_element_get_value = (function(self)
                                            {
                                              return $hs_toUtf8(self["value"]);
                                            });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_html_opt_group_element_get_type = (function()
                                              {
                                                return HTMLOptGroupElement;
                                              });
var webkit_dom_html_opt_group_element_set_disabled;
webkit_dom_html_opt_group_element_set_disabled = (function(self,
                                                           val)
                                                  {
                                                    self["disabled"] = ($hs_intToNumber(val) != 0);
                                                  });
var webkit_dom_html_opt_group_element_get_disabled;
webkit_dom_html_opt_group_element_get_disabled = (function(self)
                                                  {
                                                    return $hs_int((self["disabled"] ? 1 : 0));
                                                  });
var webkit_dom_html_opt_group_element_set_label;
webkit_dom_html_opt_group_element_set_label = (function(self, val)
                                               {
                                                 self["label"] = $hs_fromUtf8(val);
                                               });
var webkit_dom_html_opt_group_element_get_label;
webkit_dom_html_opt_group_element_get_label = (function(self)
                                               {
                                                 return $hs_toUtf8(self["label"]);
                                               });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_htmlo_list_element_get_type = (function()
                                          {
                                            return HTMLOListElement;
                                          });
var webkit_dom_htmlo_list_element_set_compact;
webkit_dom_htmlo_list_element_set_compact = (function(self, val)
                                             {
                                               self["compact"] = ($hs_intToNumber(val) != 0);
                                             });
var webkit_dom_htmlo_list_element_get_compact;
webkit_dom_htmlo_list_element_get_compact = (function(self)
                                             {
                                               return $hs_int((self["compact"] ? 1 : 0));
                                             });
var webkit_dom_htmlo_list_element_set_start;
webkit_dom_htmlo_list_element_set_start = (function(self, val)
                                           {
                                             self["start"] = $hs_intToNumber(val);
                                           });
var webkit_dom_htmlo_list_element_get_start;
webkit_dom_htmlo_list_element_get_start = (function(self)
                                           {
                                             return $hs_int(self["start"]);
                                           });
var webkit_dom_htmlo_list_element_set_reversed;
webkit_dom_htmlo_list_element_set_reversed = (function(self, val)
                                              {
                                                self["reversed"] = ($hs_intToNumber(val) != 0);
                                              });
var webkit_dom_htmlo_list_element_get_reversed;
webkit_dom_htmlo_list_element_get_reversed = (function(self)
                                              {
                                                return $hs_int((self["reversed"] ? 1 : 0));
                                              });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_html_object_element_get_type = (function()
                                           {
                                             return HTMLObjectElement;
                                           });
var webkit_dom_html_object_element_check_validity;
webkit_dom_html_object_element_check_validity = (function(self)
                                                 {
                                                   return $hs_int((self["checkValidity"]() ? 1 : 0));
                                                 });
var webkit_dom_html_object_element_set_custom_validity;
webkit_dom_html_object_element_set_custom_validity = (function(self,
                                                               error)
                                                      {
                                                        return self["setCustomValidity"]($hs_fromUtf8(error));
                                                      });
var webkit_dom_html_object_element_get_form;
webkit_dom_html_object_element_get_form = (function(self)
                                           {
                                             return self["form"];
                                           });
var webkit_dom_html_object_element_set_code;
webkit_dom_html_object_element_set_code = (function(self, val)
                                           {
                                             self["code"] = $hs_fromUtf8(val);
                                           });
var webkit_dom_html_object_element_get_code;
webkit_dom_html_object_element_get_code = (function(self)
                                           {
                                             return $hs_toUtf8(self["code"]);
                                           });
var webkit_dom_html_object_element_set_align;
webkit_dom_html_object_element_set_align = (function(self, val)
                                            {
                                              self["align"] = $hs_fromUtf8(val);
                                            });
var webkit_dom_html_object_element_get_align;
webkit_dom_html_object_element_get_align = (function(self)
                                            {
                                              return $hs_toUtf8(self["align"]);
                                            });
var webkit_dom_html_object_element_set_archive;
webkit_dom_html_object_element_set_archive = (function(self, val)
                                              {
                                                self["archive"] = $hs_fromUtf8(val);
                                              });
var webkit_dom_html_object_element_get_archive;
webkit_dom_html_object_element_get_archive = (function(self)
                                              {
                                                return $hs_toUtf8(self["archive"]);
                                              });
var webkit_dom_html_object_element_set_border;
webkit_dom_html_object_element_set_border = (function(self, val)
                                             {
                                               self["border"] = $hs_fromUtf8(val);
                                             });
var webkit_dom_html_object_element_get_border;
webkit_dom_html_object_element_get_border = (function(self)
                                             {
                                               return $hs_toUtf8(self["border"]);
                                             });
var webkit_dom_html_object_element_set_code_base;
webkit_dom_html_object_element_set_code_base = (function(self, val)
                                                {
                                                  self["codeBase"] = $hs_fromUtf8(val);
                                                });
var webkit_dom_html_object_element_get_code_base;
webkit_dom_html_object_element_get_code_base = (function(self)
                                                {
                                                  return $hs_toUtf8(self["codeBase"]);
                                                });
var webkit_dom_html_object_element_set_code_type;
webkit_dom_html_object_element_set_code_type = (function(self, val)
                                                {
                                                  self["codeType"] = $hs_fromUtf8(val);
                                                });
var webkit_dom_html_object_element_get_code_type;
webkit_dom_html_object_element_get_code_type = (function(self)
                                                {
                                                  return $hs_toUtf8(self["codeType"]);
                                                });
var webkit_dom_html_object_element_set_data;
webkit_dom_html_object_element_set_data = (function(self, val)
                                           {
                                             self["data"] = $hs_fromUtf8(val);
                                           });
var webkit_dom_html_object_element_get_data;
webkit_dom_html_object_element_get_data = (function(self)
                                           {
                                             return $hs_toUtf8(self["data"]);
                                           });
var webkit_dom_html_object_element_set_declare;
webkit_dom_html_object_element_set_declare = (function(self, val)
                                              {
                                                self["declare"] = ($hs_intToNumber(val) != 0);
                                              });
var webkit_dom_html_object_element_get_declare;
webkit_dom_html_object_element_get_declare = (function(self)
                                              {
                                                return $hs_int((self["declare"] ? 1 : 0));
                                              });
var webkit_dom_html_object_element_set_height;
webkit_dom_html_object_element_set_height = (function(self, val)
                                             {
                                               self["height"] = $hs_fromUtf8(val);
                                             });
var webkit_dom_html_object_element_get_height;
webkit_dom_html_object_element_get_height = (function(self)
                                             {
                                               return $hs_toUtf8(self["height"]);
                                             });
var webkit_dom_html_object_element_set_hspace;
webkit_dom_html_object_element_set_hspace = (function(self, val)
                                             {
                                               self["hspace"] = $hs_intToNumber(val);
                                             });
var webkit_dom_html_object_element_get_hspace;
webkit_dom_html_object_element_get_hspace = (function(self)
                                             {
                                               return $hs_int(self["hspace"]);
                                             });
var webkit_dom_html_object_element_set_name;
webkit_dom_html_object_element_set_name = (function(self, val)
                                           {
                                             self["name"] = $hs_fromUtf8(val);
                                           });
var webkit_dom_html_object_element_get_name;
webkit_dom_html_object_element_get_name = (function(self)
                                           {
                                             return $hs_toUtf8(self["name"]);
                                           });
var webkit_dom_html_object_element_set_standby;
webkit_dom_html_object_element_set_standby = (function(self, val)
                                              {
                                                self["standby"] = $hs_fromUtf8(val);
                                              });
var webkit_dom_html_object_element_get_standby;
webkit_dom_html_object_element_get_standby = (function(self)
                                              {
                                                return $hs_toUtf8(self["standby"]);
                                              });
var webkit_dom_html_object_element_set_use_map;
webkit_dom_html_object_element_set_use_map = (function(self, val)
                                              {
                                                self["useMap"] = $hs_fromUtf8(val);
                                              });
var webkit_dom_html_object_element_get_use_map;
webkit_dom_html_object_element_get_use_map = (function(self)
                                              {
                                                return $hs_toUtf8(self["useMap"]);
                                              });
var webkit_dom_html_object_element_set_vspace;
webkit_dom_html_object_element_set_vspace = (function(self, val)
                                             {
                                               self["vspace"] = $hs_intToNumber(val);
                                             });
var webkit_dom_html_object_element_get_vspace;
webkit_dom_html_object_element_get_vspace = (function(self)
                                             {
                                               return $hs_int(self["vspace"]);
                                             });
var webkit_dom_html_object_element_set_width;
webkit_dom_html_object_element_set_width = (function(self, val)
                                            {
                                              self["width"] = $hs_fromUtf8(val);
                                            });
var webkit_dom_html_object_element_get_width;
webkit_dom_html_object_element_get_width = (function(self)
                                            {
                                              return $hs_toUtf8(self["width"]);
                                            });
var webkit_dom_html_object_element_get_will_validate;
webkit_dom_html_object_element_get_will_validate = (function(self)
                                                    {
                                                      return $hs_int((self["willValidate"] ? 1 : 0));
                                                    });
var webkit_dom_html_object_element_get_validity;
webkit_dom_html_object_element_get_validity = (function(self)
                                               {
                                                 return self["validity"];
                                               });
var webkit_dom_html_object_element_get_validation_message;
webkit_dom_html_object_element_get_validation_message = (function(self)
                                                         {
                                                           return $hs_toUtf8(self["validationMessage"]);
                                                         });
var webkit_dom_html_object_element_get_content_document;
webkit_dom_html_object_element_get_content_document = (function(self)
                                                       {
                                                         return self["contentDocument"];
                                                       });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_html_mod_element_get_type = (function()
                                        {
                                          return HTMLModElement;
                                        });
var webkit_dom_html_mod_element_set_cite;
webkit_dom_html_mod_element_set_cite = (function(self, val)
                                        {
                                          self["cite"] = $hs_fromUtf8(val);
                                        });
var webkit_dom_html_mod_element_get_cite;
webkit_dom_html_mod_element_get_cite = (function(self)
                                        {
                                          return $hs_toUtf8(self["cite"]);
                                        });
var webkit_dom_html_mod_element_set_date_time;
webkit_dom_html_mod_element_set_date_time = (function(self, val)
                                             {
                                               self["dateTime"] = $hs_fromUtf8(val);
                                             });
var webkit_dom_html_mod_element_get_date_time;
webkit_dom_html_mod_element_get_date_time = (function(self)
                                             {
                                               return $hs_toUtf8(self["dateTime"]);
                                             });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_html_meta_element_get_type = (function()
                                         {
                                           return HTMLMetaElement;
                                         });
var webkit_dom_html_meta_element_set_content;
webkit_dom_html_meta_element_set_content = (function(self, val)
                                            {
                                              self["content"] = $hs_fromUtf8(val);
                                            });
var webkit_dom_html_meta_element_get_content;
webkit_dom_html_meta_element_get_content = (function(self)
                                            {
                                              return $hs_toUtf8(self["content"]);
                                            });
var webkit_dom_html_meta_element_set_http_equiv;
webkit_dom_html_meta_element_set_http_equiv = (function(self, val)
                                               {
                                                 self["httpEquiv"] = $hs_fromUtf8(val);
                                               });
var webkit_dom_html_meta_element_get_http_equiv;
webkit_dom_html_meta_element_get_http_equiv = (function(self)
                                               {
                                                 return $hs_toUtf8(self["httpEquiv"]);
                                               });
var webkit_dom_html_meta_element_set_name;
webkit_dom_html_meta_element_set_name = (function(self, val)
                                         {
                                           self["name"] = $hs_fromUtf8(val);
                                         });
var webkit_dom_html_meta_element_get_name;
webkit_dom_html_meta_element_get_name = (function(self)
                                         {
                                           return $hs_toUtf8(self["name"]);
                                         });
var webkit_dom_html_meta_element_set_scheme;
webkit_dom_html_meta_element_set_scheme = (function(self, val)
                                           {
                                             self["scheme"] = $hs_fromUtf8(val);
                                           });
var webkit_dom_html_meta_element_get_scheme;
webkit_dom_html_meta_element_get_scheme = (function(self)
                                           {
                                             return $hs_toUtf8(self["scheme"]);
                                           });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_html_menu_element_get_type = (function()
                                         {
                                           return HTMLMenuElement;
                                         });
var webkit_dom_html_menu_element_set_compact;
webkit_dom_html_menu_element_set_compact = (function(self, val)
                                            {
                                              self["compact"] = ($hs_intToNumber(val) != 0);
                                            });
var webkit_dom_html_menu_element_get_compact;
webkit_dom_html_menu_element_get_compact = (function(self)
                                            {
                                              return $hs_int((self["compact"] ? 1 : 0));
                                            });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_html_map_element_get_type = (function()
                                        {
                                          return HTMLMapElement;
                                        });
var webkit_dom_html_map_element_get_areas;
webkit_dom_html_map_element_get_areas = (function(self)
                                         {
                                           return self["areas"];
                                         });
var webkit_dom_html_map_element_set_name;
webkit_dom_html_map_element_set_name = (function(self, val)
                                        {
                                          self["name"] = $hs_fromUtf8(val);
                                        });
var webkit_dom_html_map_element_get_name;
webkit_dom_html_map_element_get_name = (function(self)
                                        {
                                          return $hs_toUtf8(self["name"]);
                                        });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_html_link_element_get_type = (function()
                                         {
                                           return HTMLLinkElement;
                                         });
var webkit_dom_html_link_element_set_disabled;
webkit_dom_html_link_element_set_disabled = (function(self, val)
                                             {
                                               self["disabled"] = ($hs_intToNumber(val) != 0);
                                             });
var webkit_dom_html_link_element_get_disabled;
webkit_dom_html_link_element_get_disabled = (function(self)
                                             {
                                               return $hs_int((self["disabled"] ? 1 : 0));
                                             });
var webkit_dom_html_link_element_set_charset;
webkit_dom_html_link_element_set_charset = (function(self, val)
                                            {
                                              self["charset"] = $hs_fromUtf8(val);
                                            });
var webkit_dom_html_link_element_get_charset;
webkit_dom_html_link_element_get_charset = (function(self)
                                            {
                                              return $hs_toUtf8(self["charset"]);
                                            });
var webkit_dom_html_link_element_set_href;
webkit_dom_html_link_element_set_href = (function(self, val)
                                         {
                                           self["href"] = $hs_fromUtf8(val);
                                         });
var webkit_dom_html_link_element_get_href;
webkit_dom_html_link_element_get_href = (function(self)
                                         {
                                           return $hs_toUtf8(self["href"]);
                                         });
var webkit_dom_html_link_element_set_hreflang;
webkit_dom_html_link_element_set_hreflang = (function(self, val)
                                             {
                                               self["hreflang"] = $hs_fromUtf8(val);
                                             });
var webkit_dom_html_link_element_get_hreflang;
webkit_dom_html_link_element_get_hreflang = (function(self)
                                             {
                                               return $hs_toUtf8(self["hreflang"]);
                                             });
var webkit_dom_html_link_element_set_media;
webkit_dom_html_link_element_set_media = (function(self, val)
                                          {
                                            self["media"] = $hs_fromUtf8(val);
                                          });
var webkit_dom_html_link_element_get_media;
webkit_dom_html_link_element_get_media = (function(self)
                                          {
                                            return $hs_toUtf8(self["media"]);
                                          });
var webkit_dom_html_link_element_set_rel;
webkit_dom_html_link_element_set_rel = (function(self, val)
                                        {
                                          self["rel"] = $hs_fromUtf8(val);
                                        });
var webkit_dom_html_link_element_get_rel;
webkit_dom_html_link_element_get_rel = (function(self)
                                        {
                                          return $hs_toUtf8(self["rel"]);
                                        });
var webkit_dom_html_link_element_set_rev;
webkit_dom_html_link_element_set_rev = (function(self, val)
                                        {
                                          self["rev"] = $hs_fromUtf8(val);
                                        });
var webkit_dom_html_link_element_get_rev;
webkit_dom_html_link_element_get_rev = (function(self)
                                        {
                                          return $hs_toUtf8(self["rev"]);
                                        });
var webkit_dom_html_link_element_set_target;
webkit_dom_html_link_element_set_target = (function(self, val)
                                           {
                                             self["target"] = $hs_fromUtf8(val);
                                           });
var webkit_dom_html_link_element_get_target;
webkit_dom_html_link_element_get_target = (function(self)
                                           {
                                             return $hs_toUtf8(self["target"]);
                                           });
var webkit_dom_html_link_element_get_sheet;
webkit_dom_html_link_element_get_sheet = (function(self)
                                          {
                                            return self["sheet"];
                                          });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_htmlli_element_get_type = (function()
                                      {
                                        return HTMLLIElement;
                                      });
var webkit_dom_htmlli_element_set_value;
webkit_dom_htmlli_element_set_value = (function(self, val)
                                       {
                                         self["value"] = $hs_intToNumber(val);
                                       });
var webkit_dom_htmlli_element_get_value;
webkit_dom_htmlli_element_get_value = (function(self)
                                       {
                                         return $hs_int(self["value"]);
                                       });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_html_legend_element_get_type = (function()
                                           {
                                             return HTMLLegendElement;
                                           });
var webkit_dom_html_legend_element_get_form;
webkit_dom_html_legend_element_get_form = (function(self)
                                           {
                                             return self["form"];
                                           });
var webkit_dom_html_legend_element_set_align;
webkit_dom_html_legend_element_set_align = (function(self, val)
                                            {
                                              self["align"] = $hs_fromUtf8(val);
                                            });
var webkit_dom_html_legend_element_get_align;
webkit_dom_html_legend_element_get_align = (function(self)
                                            {
                                              return $hs_toUtf8(self["align"]);
                                            });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_html_label_element_get_type = (function()
                                          {
                                            return HTMLLabelElement;
                                          });
var webkit_dom_html_label_element_get_form;
webkit_dom_html_label_element_get_form = (function(self)
                                          {
                                            return self["form"];
                                          });
var webkit_dom_html_label_element_set_html_for;
webkit_dom_html_label_element_set_html_for = (function(self, val)
                                              {
                                                self["htmlFor"] = $hs_fromUtf8(val);
                                              });
var webkit_dom_html_label_element_get_html_for;
webkit_dom_html_label_element_get_html_for = (function(self)
                                              {
                                                return $hs_toUtf8(self["htmlFor"]);
                                              });
var webkit_dom_html_label_element_get_control;
webkit_dom_html_label_element_get_control = (function(self)
                                             {
                                               return self["control"];
                                             });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_html_input_element_get_type = (function()
                                          {
                                            return HTMLInputElement;
                                          });
var webkit_dom_html_input_element_step_up;
webkit_dom_html_input_element_step_up = (function(self, n)
                                         {
                                           return self["stepUp"]($hs_intToNumber(n));
                                         });
var webkit_dom_html_input_element_step_down;
webkit_dom_html_input_element_step_down = (function(self, n)
                                           {
                                             return self["stepDown"]($hs_intToNumber(n));
                                           });
var webkit_dom_html_input_element_check_validity;
webkit_dom_html_input_element_check_validity = (function(self)
                                                {
                                                  return $hs_int((self["checkValidity"]() ? 1 : 0));
                                                });
var webkit_dom_html_input_element_set_custom_validity;
webkit_dom_html_input_element_set_custom_validity = (function(self,
                                                              error)
                                                     {
                                                       return self["setCustomValidity"]($hs_fromUtf8(error));
                                                     });
var webkit_dom_html_input_element_select;
webkit_dom_html_input_element_select = (function(self)
                                        {
                                          return self["select"]();
                                        });
var webkit_dom_html_input_element_set_value_for_user;
webkit_dom_html_input_element_set_value_for_user = (function(self,
                                                             value)
                                                    {
                                                      return self["setValueForUser"]($hs_fromUtf8(value));
                                                    });
var webkit_dom_html_input_element_set_default_value;
webkit_dom_html_input_element_set_default_value = (function(self,
                                                            val)
                                                   {
                                                     self["defaultValue"] = $hs_fromUtf8(val);
                                                   });
var webkit_dom_html_input_element_get_default_value;
webkit_dom_html_input_element_get_default_value = (function(self)
                                                   {
                                                     return $hs_toUtf8(self["defaultValue"]);
                                                   });
var webkit_dom_html_input_element_set_default_checked;
webkit_dom_html_input_element_set_default_checked = (function(self,
                                                              val)
                                                     {
                                                       self["defaultChecked"] = ($hs_intToNumber(val)
                                                                                 !=
                                                                                 0);
                                                     });
var webkit_dom_html_input_element_get_default_checked;
webkit_dom_html_input_element_get_default_checked = (function(self)
                                                     {
                                                       return $hs_int((self["defaultChecked"] ? 1 : 0));
                                                     });
var webkit_dom_html_input_element_set_dir_name;
webkit_dom_html_input_element_set_dir_name = (function(self, val)
                                              {
                                                self["dirName"] = $hs_fromUtf8(val);
                                              });
var webkit_dom_html_input_element_get_dir_name;
webkit_dom_html_input_element_get_dir_name = (function(self)
                                              {
                                                return $hs_toUtf8(self["dirName"]);
                                              });
var webkit_dom_html_input_element_get_form;
webkit_dom_html_input_element_get_form = (function(self)
                                          {
                                            return self["form"];
                                          });
var webkit_dom_html_input_element_set_form_action;
webkit_dom_html_input_element_set_form_action = (function(self,
                                                          val)
                                                 {
                                                   self["formAction"] = $hs_fromUtf8(val);
                                                 });
var webkit_dom_html_input_element_get_form_action;
webkit_dom_html_input_element_get_form_action = (function(self)
                                                 {
                                                   return $hs_toUtf8(self["formAction"]);
                                                 });
var webkit_dom_html_input_element_set_form_enctype;
webkit_dom_html_input_element_set_form_enctype = (function(self,
                                                           val)
                                                  {
                                                    self["formEnctype"] = $hs_fromUtf8(val);
                                                  });
var webkit_dom_html_input_element_get_form_enctype;
webkit_dom_html_input_element_get_form_enctype = (function(self)
                                                  {
                                                    return $hs_toUtf8(self["formEnctype"]);
                                                  });
var webkit_dom_html_input_element_set_form_method;
webkit_dom_html_input_element_set_form_method = (function(self,
                                                          val)
                                                 {
                                                   self["formMethod"] = $hs_fromUtf8(val);
                                                 });
var webkit_dom_html_input_element_get_form_method;
webkit_dom_html_input_element_get_form_method = (function(self)
                                                 {
                                                   return $hs_toUtf8(self["formMethod"]);
                                                 });
var webkit_dom_html_input_element_set_form_no_validate;
webkit_dom_html_input_element_set_form_no_validate = (function(self,
                                                               val)
                                                      {
                                                        self["formNoValidate"] = ($hs_intToNumber(val)
                                                                                  !=
                                                                                  0);
                                                      });
var webkit_dom_html_input_element_get_form_no_validate;
webkit_dom_html_input_element_get_form_no_validate = (function(self)
                                                      {
                                                        return $hs_int((self["formNoValidate"] ? 1 : 0));
                                                      });
var webkit_dom_html_input_element_set_form_target;
webkit_dom_html_input_element_set_form_target = (function(self,
                                                          val)
                                                 {
                                                   self["formTarget"] = $hs_fromUtf8(val);
                                                 });
var webkit_dom_html_input_element_get_form_target;
webkit_dom_html_input_element_get_form_target = (function(self)
                                                 {
                                                   return $hs_toUtf8(self["formTarget"]);
                                                 });
var webkit_dom_html_input_element_get_validity;
webkit_dom_html_input_element_get_validity = (function(self)
                                              {
                                                return self["validity"];
                                              });
var webkit_dom_html_input_element_set_accept;
webkit_dom_html_input_element_set_accept = (function(self, val)
                                            {
                                              self["accept"] = $hs_fromUtf8(val);
                                            });
var webkit_dom_html_input_element_get_accept;
webkit_dom_html_input_element_get_accept = (function(self)
                                            {
                                              return $hs_toUtf8(self["accept"]);
                                            });
var webkit_dom_html_input_element_set_align;
webkit_dom_html_input_element_set_align = (function(self, val)
                                           {
                                             self["align"] = $hs_fromUtf8(val);
                                           });
var webkit_dom_html_input_element_get_align;
webkit_dom_html_input_element_get_align = (function(self)
                                           {
                                             return $hs_toUtf8(self["align"]);
                                           });
var webkit_dom_html_input_element_set_alt;
webkit_dom_html_input_element_set_alt = (function(self, val)
                                         {
                                           self["alt"] = $hs_fromUtf8(val);
                                         });
var webkit_dom_html_input_element_get_alt;
webkit_dom_html_input_element_get_alt = (function(self)
                                         {
                                           return $hs_toUtf8(self["alt"]);
                                         });
var webkit_dom_html_input_element_set_checked;
webkit_dom_html_input_element_set_checked = (function(self, val)
                                             {
                                               self["checked"] = ($hs_intToNumber(val) != 0);
                                             });
var webkit_dom_html_input_element_get_checked;
webkit_dom_html_input_element_get_checked = (function(self)
                                             {
                                               return $hs_int((self["checked"] ? 1 : 0));
                                             });
var webkit_dom_html_input_element_set_disabled;
webkit_dom_html_input_element_set_disabled = (function(self, val)
                                              {
                                                self["disabled"] = ($hs_intToNumber(val) != 0);
                                              });
var webkit_dom_html_input_element_get_disabled;
webkit_dom_html_input_element_get_disabled = (function(self)
                                              {
                                                return $hs_int((self["disabled"] ? 1 : 0));
                                              });
var webkit_dom_html_input_element_set_autofocus;
webkit_dom_html_input_element_set_autofocus = (function(self, val)
                                               {
                                                 self["autofocus"] = ($hs_intToNumber(val) != 0);
                                               });
var webkit_dom_html_input_element_get_autofocus;
webkit_dom_html_input_element_get_autofocus = (function(self)
                                               {
                                                 return $hs_int((self["autofocus"] ? 1 : 0));
                                               });
var webkit_dom_html_input_element_set_autocomplete;
webkit_dom_html_input_element_set_autocomplete = (function(self,
                                                           val)
                                                  {
                                                    self["autocomplete"] = $hs_fromUtf8(val);
                                                  });
var webkit_dom_html_input_element_get_autocomplete;
webkit_dom_html_input_element_get_autocomplete = (function(self)
                                                  {
                                                    return $hs_toUtf8(self["autocomplete"]);
                                                  });
var webkit_dom_html_input_element_get_list;
webkit_dom_html_input_element_get_list = (function(self)
                                          {
                                            return self["list"];
                                          });
var webkit_dom_html_input_element_set_max;
webkit_dom_html_input_element_set_max = (function(self, val)
                                         {
                                           self["max"] = $hs_fromUtf8(val);
                                         });
var webkit_dom_html_input_element_get_max;
webkit_dom_html_input_element_get_max = (function(self)
                                         {
                                           return $hs_toUtf8(self["max"]);
                                         });
var webkit_dom_html_input_element_set_max_length;
webkit_dom_html_input_element_set_max_length = (function(self, val)
                                                {
                                                  self["maxLength"] = $hs_intToNumber(val);
                                                });
var webkit_dom_html_input_element_get_max_length;
webkit_dom_html_input_element_get_max_length = (function(self)
                                                {
                                                  return $hs_int(self["maxLength"]);
                                                });
var webkit_dom_html_input_element_set_min;
webkit_dom_html_input_element_set_min = (function(self, val)
                                         {
                                           self["min"] = $hs_fromUtf8(val);
                                         });
var webkit_dom_html_input_element_get_min;
webkit_dom_html_input_element_get_min = (function(self)
                                         {
                                           return $hs_toUtf8(self["min"]);
                                         });
var webkit_dom_html_input_element_set_multiple;
webkit_dom_html_input_element_set_multiple = (function(self, val)
                                              {
                                                self["multiple"] = ($hs_intToNumber(val) != 0);
                                              });
var webkit_dom_html_input_element_get_multiple;
webkit_dom_html_input_element_get_multiple = (function(self)
                                              {
                                                return $hs_int((self["multiple"] ? 1 : 0));
                                              });
var webkit_dom_html_input_element_set_webkitdirectory;
webkit_dom_html_input_element_set_webkitdirectory = (function(self,
                                                              val)
                                                     {
                                                       self["webkitdirectory"] = ($hs_intToNumber(val)
                                                                                  !=
                                                                                  0);
                                                     });
var webkit_dom_html_input_element_get_webkitdirectory;
webkit_dom_html_input_element_get_webkitdirectory = (function(self)
                                                     {
                                                       return $hs_int((self["webkitdirectory"] ? 1 : 0));
                                                     });
var webkit_dom_html_input_element_set_name;
webkit_dom_html_input_element_set_name = (function(self, val)
                                          {
                                            self["name"] = $hs_fromUtf8(val);
                                          });
var webkit_dom_html_input_element_get_name;
webkit_dom_html_input_element_get_name = (function(self)
                                          {
                                            return $hs_toUtf8(self["name"]);
                                          });
var webkit_dom_html_input_element_set_pattern;
webkit_dom_html_input_element_set_pattern = (function(self, val)
                                             {
                                               self["pattern"] = $hs_fromUtf8(val);
                                             });
var webkit_dom_html_input_element_get_pattern;
webkit_dom_html_input_element_get_pattern = (function(self)
                                             {
                                               return $hs_toUtf8(self["pattern"]);
                                             });
var webkit_dom_html_input_element_set_placeholder;
webkit_dom_html_input_element_set_placeholder = (function(self,
                                                          val)
                                                 {
                                                   self["placeholder"] = $hs_fromUtf8(val);
                                                 });
var webkit_dom_html_input_element_get_placeholder;
webkit_dom_html_input_element_get_placeholder = (function(self)
                                                 {
                                                   return $hs_toUtf8(self["placeholder"]);
                                                 });
var webkit_dom_html_input_element_set_read_only;
webkit_dom_html_input_element_set_read_only = (function(self, val)
                                               {
                                                 self["readOnly"] = ($hs_intToNumber(val) != 0);
                                               });
var webkit_dom_html_input_element_get_read_only;
webkit_dom_html_input_element_get_read_only = (function(self)
                                               {
                                                 return $hs_int((self["readOnly"] ? 1 : 0));
                                               });
var webkit_dom_html_input_element_set_required;
webkit_dom_html_input_element_set_required = (function(self, val)
                                              {
                                                self["required"] = ($hs_intToNumber(val) != 0);
                                              });
var webkit_dom_html_input_element_get_required;
webkit_dom_html_input_element_get_required = (function(self)
                                              {
                                                return $hs_int((self["required"] ? 1 : 0));
                                              });
var webkit_dom_html_input_element_set_size;
webkit_dom_html_input_element_set_size = (function(self, val)
                                          {
                                            self["size"] = $hs_intToNumber(val);
                                          });
var webkit_dom_html_input_element_get_size;
webkit_dom_html_input_element_get_size = (function(self)
                                          {
                                            return $hs_int(self["size"]);
                                          });
var webkit_dom_html_input_element_set_src;
webkit_dom_html_input_element_set_src = (function(self, val)
                                         {
                                           self["src"] = $hs_fromUtf8(val);
                                         });
var webkit_dom_html_input_element_get_src;
webkit_dom_html_input_element_get_src = (function(self)
                                         {
                                           return $hs_toUtf8(self["src"]);
                                         });
var webkit_dom_html_input_element_set_step;
webkit_dom_html_input_element_set_step = (function(self, val)
                                          {
                                            self["step"] = $hs_fromUtf8(val);
                                          });
var webkit_dom_html_input_element_get_step;
webkit_dom_html_input_element_get_step = (function(self)
                                          {
                                            return $hs_toUtf8(self["step"]);
                                          });
var webkit_dom_html_input_element_set_use_map;
webkit_dom_html_input_element_set_use_map = (function(self, val)
                                             {
                                               self["useMap"] = $hs_fromUtf8(val);
                                             });
var webkit_dom_html_input_element_get_use_map;
webkit_dom_html_input_element_get_use_map = (function(self)
                                             {
                                               return $hs_toUtf8(self["useMap"]);
                                             });
var webkit_dom_html_input_element_set_value;
webkit_dom_html_input_element_set_value = (function(self, val)
                                           {
                                             self["value"] = $hs_fromUtf8(val);
                                           });
var webkit_dom_html_input_element_get_value;
webkit_dom_html_input_element_get_value = (function(self)
                                           {
                                             return $hs_toUtf8(self["value"]);
                                           });
var webkit_dom_html_input_element_set_value_as_number;
webkit_dom_html_input_element_set_value_as_number = (function(self,
                                                              val)
                                                     {
                                                       self["valueAsNumber"] = val;
                                                     });
var webkit_dom_html_input_element_get_value_as_number;
webkit_dom_html_input_element_get_value_as_number = (function(self)
                                                     {
                                                       return self["valueAsNumber"];
                                                     });
var webkit_dom_html_input_element_get_selected_option;
webkit_dom_html_input_element_get_selected_option = (function(self)
                                                     {
                                                       return self["selectedOption"];
                                                     });
var webkit_dom_html_input_element_set_incremental;
webkit_dom_html_input_element_set_incremental = (function(self,
                                                          val)
                                                 {
                                                   self["incremental"] = ($hs_intToNumber(val)
                                                                          !=
                                                                          0);
                                                 });
var webkit_dom_html_input_element_get_incremental;
webkit_dom_html_input_element_get_incremental = (function(self)
                                                 {
                                                   return $hs_int((self["incremental"] ? 1 : 0));
                                                 });
var webkit_dom_html_input_element_get_will_validate;
webkit_dom_html_input_element_get_will_validate = (function(self)
                                                   {
                                                     return $hs_int((self["willValidate"] ? 1 : 0));
                                                   });
var webkit_dom_html_input_element_get_validation_message;
webkit_dom_html_input_element_get_validation_message = (function(self)
                                                        {
                                                          return $hs_toUtf8(self["validationMessage"]);
                                                        });
var webkit_dom_html_input_element_set_indeterminate;
webkit_dom_html_input_element_set_indeterminate = (function(self,
                                                            val)
                                                   {
                                                     self["indeterminate"] = ($hs_intToNumber(val)
                                                                              !=
                                                                              0);
                                                   });
var webkit_dom_html_input_element_get_indeterminate;
webkit_dom_html_input_element_get_indeterminate = (function(self)
                                                   {
                                                     return $hs_int((self["indeterminate"] ? 1 : 0));
                                                   });
var webkit_dom_html_input_element_get_files;
webkit_dom_html_input_element_get_files = (function(self)
                                           {
                                             return self["files"];
                                           });
var webkit_dom_html_input_element_get_labels;
webkit_dom_html_input_element_get_labels = (function(self)
                                            {
                                              return self["labels"];
                                            });
var webkit_dom_html_input_element_set_webkit_speech;
webkit_dom_html_input_element_set_webkit_speech = (function(self,
                                                            val)
                                                   {
                                                     self["webkitSpeech"] = ($hs_intToNumber(val)
                                                                             !=
                                                                             0);
                                                   });
var webkit_dom_html_input_element_get_webkit_speech;
webkit_dom_html_input_element_get_webkit_speech = (function(self)
                                                   {
                                                     return $hs_int((self["webkitSpeech"] ? 1 : 0));
                                                   });
var webkit_dom_html_input_element_set_webkit_grammar;
webkit_dom_html_input_element_set_webkit_grammar = (function(self,
                                                             val)
                                                    {
                                                      self["webkitGrammar"] = ($hs_intToNumber(val)
                                                                               !=
                                                                               0);
                                                    });
var webkit_dom_html_input_element_get_webkit_grammar;
webkit_dom_html_input_element_get_webkit_grammar = (function(self)
                                                    {
                                                      return $hs_int((self["webkitGrammar"] ? 1 : 0));
                                                    });
var webkit_dom_html_input_element_set_onwebkitspeechchange;
webkit_dom_html_input_element_set_onwebkitspeechchange = (function(self,
                                                                   val)
                                                          {
                                                            self["onwebkitspeechchange"] = val;
                                                          });
var webkit_dom_html_input_element_get_onwebkitspeechchange;
webkit_dom_html_input_element_get_onwebkitspeechchange = (function(self)
                                                          {
                                                            return self["onwebkitspeechchange"];
                                                          });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_html_image_element_get_type = (function()
                                          {
                                            return HTMLImageElement;
                                          });
var webkit_dom_html_image_element_set_name;
webkit_dom_html_image_element_set_name = (function(self, val)
                                          {
                                            self["name"] = $hs_fromUtf8(val);
                                          });
var webkit_dom_html_image_element_get_name;
webkit_dom_html_image_element_get_name = (function(self)
                                          {
                                            return $hs_toUtf8(self["name"]);
                                          });
var webkit_dom_html_image_element_set_align;
webkit_dom_html_image_element_set_align = (function(self, val)
                                           {
                                             self["align"] = $hs_fromUtf8(val);
                                           });
var webkit_dom_html_image_element_get_align;
webkit_dom_html_image_element_get_align = (function(self)
                                           {
                                             return $hs_toUtf8(self["align"]);
                                           });
var webkit_dom_html_image_element_set_alt;
webkit_dom_html_image_element_set_alt = (function(self, val)
                                         {
                                           self["alt"] = $hs_fromUtf8(val);
                                         });
var webkit_dom_html_image_element_get_alt;
webkit_dom_html_image_element_get_alt = (function(self)
                                         {
                                           return $hs_toUtf8(self["alt"]);
                                         });
var webkit_dom_html_image_element_set_border;
webkit_dom_html_image_element_set_border = (function(self, val)
                                            {
                                              self["border"] = $hs_fromUtf8(val);
                                            });
var webkit_dom_html_image_element_get_border;
webkit_dom_html_image_element_get_border = (function(self)
                                            {
                                              return $hs_toUtf8(self["border"]);
                                            });
var webkit_dom_html_image_element_set_cross_origin;
webkit_dom_html_image_element_set_cross_origin = (function(self,
                                                           val)
                                                  {
                                                    self["crossOrigin"] = $hs_fromUtf8(val);
                                                  });
var webkit_dom_html_image_element_get_cross_origin;
webkit_dom_html_image_element_get_cross_origin = (function(self)
                                                  {
                                                    return $hs_toUtf8(self["crossOrigin"]);
                                                  });
var webkit_dom_html_image_element_set_height;
webkit_dom_html_image_element_set_height = (function(self, val)
                                            {
                                              self["height"] = $hs_intToNumber(val);
                                            });
var webkit_dom_html_image_element_get_height;
webkit_dom_html_image_element_get_height = (function(self)
                                            {
                                              return $hs_int(self["height"]);
                                            });
var webkit_dom_html_image_element_set_hspace;
webkit_dom_html_image_element_set_hspace = (function(self, val)
                                            {
                                              self["hspace"] = $hs_intToNumber(val);
                                            });
var webkit_dom_html_image_element_get_hspace;
webkit_dom_html_image_element_get_hspace = (function(self)
                                            {
                                              return $hs_int(self["hspace"]);
                                            });
var webkit_dom_html_image_element_set_is_map;
webkit_dom_html_image_element_set_is_map = (function(self, val)
                                            {
                                              self["isMap"] = ($hs_intToNumber(val) != 0);
                                            });
var webkit_dom_html_image_element_get_is_map;
webkit_dom_html_image_element_get_is_map = (function(self)
                                            {
                                              return $hs_int((self["isMap"] ? 1 : 0));
                                            });
var webkit_dom_html_image_element_set_long_desc;
webkit_dom_html_image_element_set_long_desc = (function(self, val)
                                               {
                                                 self["longDesc"] = $hs_fromUtf8(val);
                                               });
var webkit_dom_html_image_element_get_long_desc;
webkit_dom_html_image_element_get_long_desc = (function(self)
                                               {
                                                 return $hs_toUtf8(self["longDesc"]);
                                               });
var webkit_dom_html_image_element_set_src;
webkit_dom_html_image_element_set_src = (function(self, val)
                                         {
                                           self["src"] = $hs_fromUtf8(val);
                                         });
var webkit_dom_html_image_element_get_src;
webkit_dom_html_image_element_get_src = (function(self)
                                         {
                                           return $hs_toUtf8(self["src"]);
                                         });
var webkit_dom_html_image_element_set_use_map;
webkit_dom_html_image_element_set_use_map = (function(self, val)
                                             {
                                               self["useMap"] = $hs_fromUtf8(val);
                                             });
var webkit_dom_html_image_element_get_use_map;
webkit_dom_html_image_element_get_use_map = (function(self)
                                             {
                                               return $hs_toUtf8(self["useMap"]);
                                             });
var webkit_dom_html_image_element_set_vspace;
webkit_dom_html_image_element_set_vspace = (function(self, val)
                                            {
                                              self["vspace"] = $hs_intToNumber(val);
                                            });
var webkit_dom_html_image_element_get_vspace;
webkit_dom_html_image_element_get_vspace = (function(self)
                                            {
                                              return $hs_int(self["vspace"]);
                                            });
var webkit_dom_html_image_element_set_width;
webkit_dom_html_image_element_set_width = (function(self, val)
                                           {
                                             self["width"] = $hs_intToNumber(val);
                                           });
var webkit_dom_html_image_element_get_width;
webkit_dom_html_image_element_get_width = (function(self)
                                           {
                                             return $hs_int(self["width"]);
                                           });
var webkit_dom_html_image_element_get_complete;
webkit_dom_html_image_element_get_complete = (function(self)
                                              {
                                                return $hs_int((self["complete"] ? 1 : 0));
                                              });
var webkit_dom_html_image_element_set_lowsrc;
webkit_dom_html_image_element_set_lowsrc = (function(self, val)
                                            {
                                              self["lowsrc"] = $hs_fromUtf8(val);
                                            });
var webkit_dom_html_image_element_get_lowsrc;
webkit_dom_html_image_element_get_lowsrc = (function(self)
                                            {
                                              return $hs_toUtf8(self["lowsrc"]);
                                            });
var webkit_dom_html_image_element_get_natural_height;
webkit_dom_html_image_element_get_natural_height = (function(self)
                                                    {
                                                      return $hs_int(self["naturalHeight"]);
                                                    });
var webkit_dom_html_image_element_get_natural_width;
webkit_dom_html_image_element_get_natural_width = (function(self)
                                                   {
                                                     return $hs_int(self["naturalWidth"]);
                                                   });
var webkit_dom_html_image_element_get_x;
webkit_dom_html_image_element_get_x = (function(self)
                                       {
                                         return $hs_int(self["x"]);
                                       });
var webkit_dom_html_image_element_get_y;
webkit_dom_html_image_element_get_y = (function(self)
                                       {
                                         return $hs_int(self["y"]);
                                       });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_html_iframe_element_get_type = (function()
                                           {
                                             return HTMLIFrameElement;
                                           });
var webkit_dom_html_iframe_element_set_align;
webkit_dom_html_iframe_element_set_align = (function(self, val)
                                            {
                                              self["align"] = $hs_fromUtf8(val);
                                            });
var webkit_dom_html_iframe_element_get_align;
webkit_dom_html_iframe_element_get_align = (function(self)
                                            {
                                              return $hs_toUtf8(self["align"]);
                                            });
var webkit_dom_html_iframe_element_set_frame_border;
webkit_dom_html_iframe_element_set_frame_border = (function(self,
                                                            val)
                                                   {
                                                     self["frameBorder"] = $hs_fromUtf8(val);
                                                   });
var webkit_dom_html_iframe_element_get_frame_border;
webkit_dom_html_iframe_element_get_frame_border = (function(self)
                                                   {
                                                     return $hs_toUtf8(self["frameBorder"]);
                                                   });
var webkit_dom_html_iframe_element_set_height;
webkit_dom_html_iframe_element_set_height = (function(self, val)
                                             {
                                               self["height"] = $hs_fromUtf8(val);
                                             });
var webkit_dom_html_iframe_element_get_height;
webkit_dom_html_iframe_element_get_height = (function(self)
                                             {
                                               return $hs_toUtf8(self["height"]);
                                             });
var webkit_dom_html_iframe_element_set_long_desc;
webkit_dom_html_iframe_element_set_long_desc = (function(self, val)
                                                {
                                                  self["longDesc"] = $hs_fromUtf8(val);
                                                });
var webkit_dom_html_iframe_element_get_long_desc;
webkit_dom_html_iframe_element_get_long_desc = (function(self)
                                                {
                                                  return $hs_toUtf8(self["longDesc"]);
                                                });
var webkit_dom_html_iframe_element_set_margin_height;
webkit_dom_html_iframe_element_set_margin_height = (function(self,
                                                             val)
                                                    {
                                                      self["marginHeight"] = $hs_fromUtf8(val);
                                                    });
var webkit_dom_html_iframe_element_get_margin_height;
webkit_dom_html_iframe_element_get_margin_height = (function(self)
                                                    {
                                                      return $hs_toUtf8(self["marginHeight"]);
                                                    });
var webkit_dom_html_iframe_element_set_margin_width;
webkit_dom_html_iframe_element_set_margin_width = (function(self,
                                                            val)
                                                   {
                                                     self["marginWidth"] = $hs_fromUtf8(val);
                                                   });
var webkit_dom_html_iframe_element_get_margin_width;
webkit_dom_html_iframe_element_get_margin_width = (function(self)
                                                   {
                                                     return $hs_toUtf8(self["marginWidth"]);
                                                   });
var webkit_dom_html_iframe_element_set_name;
webkit_dom_html_iframe_element_set_name = (function(self, val)
                                           {
                                             self["name"] = $hs_fromUtf8(val);
                                           });
var webkit_dom_html_iframe_element_get_name;
webkit_dom_html_iframe_element_get_name = (function(self)
                                           {
                                             return $hs_toUtf8(self["name"]);
                                           });
var webkit_dom_html_iframe_element_set_sandbox;
webkit_dom_html_iframe_element_set_sandbox = (function(self, val)
                                              {
                                                self["sandbox"] = $hs_fromUtf8(val);
                                              });
var webkit_dom_html_iframe_element_get_sandbox;
webkit_dom_html_iframe_element_get_sandbox = (function(self)
                                              {
                                                return $hs_toUtf8(self["sandbox"]);
                                              });
var webkit_dom_html_iframe_element_set_scrolling;
webkit_dom_html_iframe_element_set_scrolling = (function(self, val)
                                                {
                                                  self["scrolling"] = $hs_fromUtf8(val);
                                                });
var webkit_dom_html_iframe_element_get_scrolling;
webkit_dom_html_iframe_element_get_scrolling = (function(self)
                                                {
                                                  return $hs_toUtf8(self["scrolling"]);
                                                });
var webkit_dom_html_iframe_element_set_src;
webkit_dom_html_iframe_element_set_src = (function(self, val)
                                          {
                                            self["src"] = $hs_fromUtf8(val);
                                          });
var webkit_dom_html_iframe_element_get_src;
webkit_dom_html_iframe_element_get_src = (function(self)
                                          {
                                            return $hs_toUtf8(self["src"]);
                                          });
var webkit_dom_html_iframe_element_set_width;
webkit_dom_html_iframe_element_set_width = (function(self, val)
                                            {
                                              self["width"] = $hs_fromUtf8(val);
                                            });
var webkit_dom_html_iframe_element_get_width;
webkit_dom_html_iframe_element_get_width = (function(self)
                                            {
                                              return $hs_toUtf8(self["width"]);
                                            });
var webkit_dom_html_iframe_element_get_content_document;
webkit_dom_html_iframe_element_get_content_document = (function(self)
                                                       {
                                                         return self["contentDocument"];
                                                       });
var webkit_dom_html_iframe_element_get_content_window;
webkit_dom_html_iframe_element_get_content_window = (function(self)
                                                     {
                                                       return self["contentWindow"];
                                                     });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_html_html_element_get_type = (function()
                                         {
                                           return HTMLHtmlElement;
                                         });
var webkit_dom_html_html_element_set_version;
webkit_dom_html_html_element_set_version = (function(self, val)
                                            {
                                              self["version"] = $hs_fromUtf8(val);
                                            });
var webkit_dom_html_html_element_get_version;
webkit_dom_html_html_element_get_version = (function(self)
                                            {
                                              return $hs_toUtf8(self["version"]);
                                            });
var webkit_dom_html_html_element_set_manifest;
webkit_dom_html_html_element_set_manifest = (function(self, val)
                                             {
                                               self["manifest"] = $hs_fromUtf8(val);
                                             });
var webkit_dom_html_html_element_get_manifest;
webkit_dom_html_html_element_get_manifest = (function(self)
                                             {
                                               return $hs_toUtf8(self["manifest"]);
                                             });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_htmlhr_element_get_type = (function()
                                      {
                                        return HTMLHRElement;
                                      });
var webkit_dom_htmlhr_element_set_align;
webkit_dom_htmlhr_element_set_align = (function(self, val)
                                       {
                                         self["align"] = $hs_fromUtf8(val);
                                       });
var webkit_dom_htmlhr_element_get_align;
webkit_dom_htmlhr_element_get_align = (function(self)
                                       {
                                         return $hs_toUtf8(self["align"]);
                                       });
var webkit_dom_htmlhr_element_set_no_shade;
webkit_dom_htmlhr_element_set_no_shade = (function(self, val)
                                          {
                                            self["noShade"] = ($hs_intToNumber(val) != 0);
                                          });
var webkit_dom_htmlhr_element_get_no_shade;
webkit_dom_htmlhr_element_get_no_shade = (function(self)
                                          {
                                            return $hs_int((self["noShade"] ? 1 : 0));
                                          });
var webkit_dom_htmlhr_element_set_size;
webkit_dom_htmlhr_element_set_size = (function(self, val)
                                      {
                                        self["size"] = $hs_fromUtf8(val);
                                      });
var webkit_dom_htmlhr_element_get_size;
webkit_dom_htmlhr_element_get_size = (function(self)
                                      {
                                        return $hs_toUtf8(self["size"]);
                                      });
var webkit_dom_htmlhr_element_set_width;
webkit_dom_htmlhr_element_set_width = (function(self, val)
                                       {
                                         self["width"] = $hs_fromUtf8(val);
                                       });
var webkit_dom_htmlhr_element_get_width;
webkit_dom_htmlhr_element_get_width = (function(self)
                                       {
                                         return $hs_toUtf8(self["width"]);
                                       });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_html_heading_element_get_type = (function()
                                            {
                                              return HTMLHeadingElement;
                                            });
var webkit_dom_html_heading_element_set_align;
webkit_dom_html_heading_element_set_align = (function(self, val)
                                             {
                                               self["align"] = $hs_fromUtf8(val);
                                             });
var webkit_dom_html_heading_element_get_align;
webkit_dom_html_heading_element_get_align = (function(self)
                                             {
                                               return $hs_toUtf8(self["align"]);
                                             });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_html_head_element_get_type = (function()
                                         {
                                           return HTMLHeadElement;
                                         });
var webkit_dom_html_head_element_set_profile;
webkit_dom_html_head_element_set_profile = (function(self, val)
                                            {
                                              self["profile"] = $hs_fromUtf8(val);
                                            });
var webkit_dom_html_head_element_get_profile;
webkit_dom_html_head_element_get_profile = (function(self)
                                            {
                                              return $hs_toUtf8(self["profile"]);
                                            });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_html_frame_set_element_get_type = (function()
                                              {
                                                return HTMLFrameSetElement;
                                              });
var webkit_dom_html_frame_set_element_set_cols;
webkit_dom_html_frame_set_element_set_cols = (function(self, val)
                                              {
                                                self["cols"] = $hs_fromUtf8(val);
                                              });
var webkit_dom_html_frame_set_element_get_cols;
webkit_dom_html_frame_set_element_get_cols = (function(self)
                                              {
                                                return $hs_toUtf8(self["cols"]);
                                              });
var webkit_dom_html_frame_set_element_set_rows;
webkit_dom_html_frame_set_element_set_rows = (function(self, val)
                                              {
                                                self["rows"] = $hs_fromUtf8(val);
                                              });
var webkit_dom_html_frame_set_element_get_rows;
webkit_dom_html_frame_set_element_get_rows = (function(self)
                                              {
                                                return $hs_toUtf8(self["rows"]);
                                              });
var webkit_dom_html_frame_set_element_set_onbeforeunload;
webkit_dom_html_frame_set_element_set_onbeforeunload = (function(self,
                                                                 val)
                                                        {
                                                          self["onbeforeunload"] = val;
                                                        });
var webkit_dom_html_frame_set_element_get_onbeforeunload;
webkit_dom_html_frame_set_element_get_onbeforeunload = (function(self)
                                                        {
                                                          return self["onbeforeunload"];
                                                        });
var webkit_dom_html_frame_set_element_set_onhashchange;
webkit_dom_html_frame_set_element_set_onhashchange = (function(self,
                                                               val)
                                                      {
                                                        self["onhashchange"] = val;
                                                      });
var webkit_dom_html_frame_set_element_get_onhashchange;
webkit_dom_html_frame_set_element_get_onhashchange = (function(self)
                                                      {
                                                        return self["onhashchange"];
                                                      });
var webkit_dom_html_frame_set_element_set_onmessage;
webkit_dom_html_frame_set_element_set_onmessage = (function(self,
                                                            val)
                                                   {
                                                     self["onmessage"] = val;
                                                   });
var webkit_dom_html_frame_set_element_get_onmessage;
webkit_dom_html_frame_set_element_get_onmessage = (function(self)
                                                   {
                                                     return self["onmessage"];
                                                   });
var webkit_dom_html_frame_set_element_set_onoffline;
webkit_dom_html_frame_set_element_set_onoffline = (function(self,
                                                            val)
                                                   {
                                                     self["onoffline"] = val;
                                                   });
var webkit_dom_html_frame_set_element_get_onoffline;
webkit_dom_html_frame_set_element_get_onoffline = (function(self)
                                                   {
                                                     return self["onoffline"];
                                                   });
var webkit_dom_html_frame_set_element_set_ononline;
webkit_dom_html_frame_set_element_set_ononline = (function(self,
                                                           val)
                                                  {
                                                    self["ononline"] = val;
                                                  });
var webkit_dom_html_frame_set_element_get_ononline;
webkit_dom_html_frame_set_element_get_ononline = (function(self)
                                                  {
                                                    return self["ononline"];
                                                  });
var webkit_dom_html_frame_set_element_set_onpopstate;
webkit_dom_html_frame_set_element_set_onpopstate = (function(self,
                                                             val)
                                                    {
                                                      self["onpopstate"] = val;
                                                    });
var webkit_dom_html_frame_set_element_get_onpopstate;
webkit_dom_html_frame_set_element_get_onpopstate = (function(self)
                                                    {
                                                      return self["onpopstate"];
                                                    });
var webkit_dom_html_frame_set_element_set_onresize;
webkit_dom_html_frame_set_element_set_onresize = (function(self,
                                                           val)
                                                  {
                                                    self["onresize"] = val;
                                                  });
var webkit_dom_html_frame_set_element_get_onresize;
webkit_dom_html_frame_set_element_get_onresize = (function(self)
                                                  {
                                                    return self["onresize"];
                                                  });
var webkit_dom_html_frame_set_element_set_onstorage;
webkit_dom_html_frame_set_element_set_onstorage = (function(self,
                                                            val)
                                                   {
                                                     self["onstorage"] = val;
                                                   });
var webkit_dom_html_frame_set_element_get_onstorage;
webkit_dom_html_frame_set_element_get_onstorage = (function(self)
                                                   {
                                                     return self["onstorage"];
                                                   });
var webkit_dom_html_frame_set_element_set_onunload;
webkit_dom_html_frame_set_element_set_onunload = (function(self,
                                                           val)
                                                  {
                                                    self["onunload"] = val;
                                                  });
var webkit_dom_html_frame_set_element_get_onunload;
webkit_dom_html_frame_set_element_get_onunload = (function(self)
                                                  {
                                                    return self["onunload"];
                                                  });
var webkit_dom_html_frame_set_element_set_onorientationchange;
webkit_dom_html_frame_set_element_set_onorientationchange = (function(self,
                                                                      val)
                                                             {
                                                               self["onorientationchange"] = val;
                                                             });
var webkit_dom_html_frame_set_element_get_onorientationchange;
webkit_dom_html_frame_set_element_get_onorientationchange = (function(self)
                                                             {
                                                               return self["onorientationchange"];
                                                             });
var webkit_dom_html_frame_set_element_set_onblur;
webkit_dom_html_frame_set_element_set_onblur = (function(self, val)
                                                {
                                                  self["onblur"] = val;
                                                });
var webkit_dom_html_frame_set_element_get_onblur;
webkit_dom_html_frame_set_element_get_onblur = (function(self)
                                                {
                                                  return self["onblur"];
                                                });
var webkit_dom_html_frame_set_element_set_onerror;
webkit_dom_html_frame_set_element_set_onerror = (function(self,
                                                          val)
                                                 {
                                                   self["onerror"] = val;
                                                 });
var webkit_dom_html_frame_set_element_get_onerror;
webkit_dom_html_frame_set_element_get_onerror = (function(self)
                                                 {
                                                   return self["onerror"];
                                                 });
var webkit_dom_html_frame_set_element_set_onfocus;
webkit_dom_html_frame_set_element_set_onfocus = (function(self,
                                                          val)
                                                 {
                                                   self["onfocus"] = val;
                                                 });
var webkit_dom_html_frame_set_element_get_onfocus;
webkit_dom_html_frame_set_element_get_onfocus = (function(self)
                                                 {
                                                   return self["onfocus"];
                                                 });
var webkit_dom_html_frame_set_element_set_onload;
webkit_dom_html_frame_set_element_set_onload = (function(self, val)
                                                {
                                                  self["onload"] = val;
                                                });
var webkit_dom_html_frame_set_element_get_onload;
webkit_dom_html_frame_set_element_get_onload = (function(self)
                                                {
                                                  return self["onload"];
                                                });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_html_frame_element_get_type = (function()
                                          {
                                            return HTMLFrameElement;
                                          });
var webkit_dom_html_frame_element_set_frame_border;
webkit_dom_html_frame_element_set_frame_border = (function(self,
                                                           val)
                                                  {
                                                    self["frameBorder"] = $hs_fromUtf8(val);
                                                  });
var webkit_dom_html_frame_element_get_frame_border;
webkit_dom_html_frame_element_get_frame_border = (function(self)
                                                  {
                                                    return $hs_toUtf8(self["frameBorder"]);
                                                  });
var webkit_dom_html_frame_element_set_long_desc;
webkit_dom_html_frame_element_set_long_desc = (function(self, val)
                                               {
                                                 self["longDesc"] = $hs_fromUtf8(val);
                                               });
var webkit_dom_html_frame_element_get_long_desc;
webkit_dom_html_frame_element_get_long_desc = (function(self)
                                               {
                                                 return $hs_toUtf8(self["longDesc"]);
                                               });
var webkit_dom_html_frame_element_set_margin_height;
webkit_dom_html_frame_element_set_margin_height = (function(self,
                                                            val)
                                                   {
                                                     self["marginHeight"] = $hs_fromUtf8(val);
                                                   });
var webkit_dom_html_frame_element_get_margin_height;
webkit_dom_html_frame_element_get_margin_height = (function(self)
                                                   {
                                                     return $hs_toUtf8(self["marginHeight"]);
                                                   });
var webkit_dom_html_frame_element_set_margin_width;
webkit_dom_html_frame_element_set_margin_width = (function(self,
                                                           val)
                                                  {
                                                    self["marginWidth"] = $hs_fromUtf8(val);
                                                  });
var webkit_dom_html_frame_element_get_margin_width;
webkit_dom_html_frame_element_get_margin_width = (function(self)
                                                  {
                                                    return $hs_toUtf8(self["marginWidth"]);
                                                  });
var webkit_dom_html_frame_element_set_name;
webkit_dom_html_frame_element_set_name = (function(self, val)
                                          {
                                            self["name"] = $hs_fromUtf8(val);
                                          });
var webkit_dom_html_frame_element_get_name;
webkit_dom_html_frame_element_get_name = (function(self)
                                          {
                                            return $hs_toUtf8(self["name"]);
                                          });
var webkit_dom_html_frame_element_set_no_resize;
webkit_dom_html_frame_element_set_no_resize = (function(self, val)
                                               {
                                                 self["noResize"] = ($hs_intToNumber(val) != 0);
                                               });
var webkit_dom_html_frame_element_get_no_resize;
webkit_dom_html_frame_element_get_no_resize = (function(self)
                                               {
                                                 return $hs_int((self["noResize"] ? 1 : 0));
                                               });
var webkit_dom_html_frame_element_set_scrolling;
webkit_dom_html_frame_element_set_scrolling = (function(self, val)
                                               {
                                                 self["scrolling"] = $hs_fromUtf8(val);
                                               });
var webkit_dom_html_frame_element_get_scrolling;
webkit_dom_html_frame_element_get_scrolling = (function(self)
                                               {
                                                 return $hs_toUtf8(self["scrolling"]);
                                               });
var webkit_dom_html_frame_element_set_src;
webkit_dom_html_frame_element_set_src = (function(self, val)
                                         {
                                           self["src"] = $hs_fromUtf8(val);
                                         });
var webkit_dom_html_frame_element_get_src;
webkit_dom_html_frame_element_get_src = (function(self)
                                         {
                                           return $hs_toUtf8(self["src"]);
                                         });
var webkit_dom_html_frame_element_get_content_document;
webkit_dom_html_frame_element_get_content_document = (function(self)
                                                      {
                                                        return self["contentDocument"];
                                                      });
var webkit_dom_html_frame_element_get_content_window;
webkit_dom_html_frame_element_get_content_window = (function(self)
                                                    {
                                                      return self["contentWindow"];
                                                    });
var webkit_dom_html_frame_element_get_width;
webkit_dom_html_frame_element_get_width = (function(self)
                                           {
                                             return $hs_int(self["width"]);
                                           });
var webkit_dom_html_frame_element_get_height;
webkit_dom_html_frame_element_get_height = (function(self)
                                            {
                                              return $hs_int(self["height"]);
                                            });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_html_form_element_get_type = (function()
                                         {
                                           return HTMLFormElement;
                                         });
var webkit_dom_html_form_element_submit;
webkit_dom_html_form_element_submit = (function(self)
                                       {
                                         return self["submit"]();
                                       });
var webkit_dom_html_form_element_reset;
webkit_dom_html_form_element_reset = (function(self)
                                      {
                                        return self["reset"]();
                                      });
var webkit_dom_html_form_element_check_validity;
webkit_dom_html_form_element_check_validity = (function(self)
                                               {
                                                 return $hs_int((self["checkValidity"]() ? 1 : 0));
                                               });
var webkit_dom_html_form_element_get_elements;
webkit_dom_html_form_element_get_elements = (function(self)
                                             {
                                               return self["elements"];
                                             });
var webkit_dom_html_form_element_get_length;
webkit_dom_html_form_element_get_length = (function(self)
                                           {
                                             return $hs_int(self["length"]);
                                           });
var webkit_dom_html_form_element_set_name;
webkit_dom_html_form_element_set_name = (function(self, val)
                                         {
                                           self["name"] = $hs_fromUtf8(val);
                                         });
var webkit_dom_html_form_element_get_name;
webkit_dom_html_form_element_get_name = (function(self)
                                         {
                                           return $hs_toUtf8(self["name"]);
                                         });
var webkit_dom_html_form_element_set_no_validate;
webkit_dom_html_form_element_set_no_validate = (function(self, val)
                                                {
                                                  self["noValidate"] = ($hs_intToNumber(val) != 0);
                                                });
var webkit_dom_html_form_element_get_no_validate;
webkit_dom_html_form_element_get_no_validate = (function(self)
                                                {
                                                  return $hs_int((self["noValidate"] ? 1 : 0));
                                                });
var webkit_dom_html_form_element_set_accept_charset;
webkit_dom_html_form_element_set_accept_charset = (function(self,
                                                            val)
                                                   {
                                                     self["acceptCharset"] = $hs_fromUtf8(val);
                                                   });
var webkit_dom_html_form_element_get_accept_charset;
webkit_dom_html_form_element_get_accept_charset = (function(self)
                                                   {
                                                     return $hs_toUtf8(self["acceptCharset"]);
                                                   });
var webkit_dom_html_form_element_set_action;
webkit_dom_html_form_element_set_action = (function(self, val)
                                           {
                                             self["action"] = $hs_fromUtf8(val);
                                           });
var webkit_dom_html_form_element_get_action;
webkit_dom_html_form_element_get_action = (function(self)
                                           {
                                             return $hs_toUtf8(self["action"]);
                                           });
var webkit_dom_html_form_element_set_encoding;
webkit_dom_html_form_element_set_encoding = (function(self, val)
                                             {
                                               self["encoding"] = $hs_fromUtf8(val);
                                             });
var webkit_dom_html_form_element_get_encoding;
webkit_dom_html_form_element_get_encoding = (function(self)
                                             {
                                               return $hs_toUtf8(self["encoding"]);
                                             });
var webkit_dom_html_form_element_set_enctype;
webkit_dom_html_form_element_set_enctype = (function(self, val)
                                            {
                                              self["enctype"] = $hs_fromUtf8(val);
                                            });
var webkit_dom_html_form_element_get_enctype;
webkit_dom_html_form_element_get_enctype = (function(self)
                                            {
                                              return $hs_toUtf8(self["enctype"]);
                                            });
var webkit_dom_html_form_element_set_method;
webkit_dom_html_form_element_set_method = (function(self, val)
                                           {
                                             self["method"] = $hs_fromUtf8(val);
                                           });
var webkit_dom_html_form_element_get_method;
webkit_dom_html_form_element_get_method = (function(self)
                                           {
                                             return $hs_toUtf8(self["method"]);
                                           });
var webkit_dom_html_form_element_set_target;
webkit_dom_html_form_element_set_target = (function(self, val)
                                           {
                                             self["target"] = $hs_fromUtf8(val);
                                           });
var webkit_dom_html_form_element_get_target;
webkit_dom_html_form_element_get_target = (function(self)
                                           {
                                             return $hs_toUtf8(self["target"]);
                                           });
var webkit_dom_html_form_element_set_autocomplete;
webkit_dom_html_form_element_set_autocomplete = (function(self,
                                                          val)
                                                 {
                                                   self["autocomplete"] = $hs_fromUtf8(val);
                                                 });
var webkit_dom_html_form_element_get_autocomplete;
webkit_dom_html_form_element_get_autocomplete = (function(self)
                                                 {
                                                   return $hs_toUtf8(self["autocomplete"]);
                                                 });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_html_font_element_get_type = (function()
                                         {
                                           return HTMLFontElement;
                                         });
var webkit_dom_html_font_element_set_color;
webkit_dom_html_font_element_set_color = (function(self, val)
                                          {
                                            self["color"] = $hs_fromUtf8(val);
                                          });
var webkit_dom_html_font_element_get_color;
webkit_dom_html_font_element_get_color = (function(self)
                                          {
                                            return $hs_toUtf8(self["color"]);
                                          });
var webkit_dom_html_font_element_set_face;
webkit_dom_html_font_element_set_face = (function(self, val)
                                         {
                                           self["face"] = $hs_fromUtf8(val);
                                         });
var webkit_dom_html_font_element_get_face;
webkit_dom_html_font_element_get_face = (function(self)
                                         {
                                           return $hs_toUtf8(self["face"]);
                                         });
var webkit_dom_html_font_element_set_size;
webkit_dom_html_font_element_set_size = (function(self, val)
                                         {
                                           self["size"] = $hs_fromUtf8(val);
                                         });
var webkit_dom_html_font_element_get_size;
webkit_dom_html_font_element_get_size = (function(self)
                                         {
                                           return $hs_toUtf8(self["size"]);
                                         });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_html_field_set_element_get_type = (function()
                                              {
                                                return HTMLFieldSetElement;
                                              });
var webkit_dom_html_field_set_element_check_validity;
webkit_dom_html_field_set_element_check_validity = (function(self)
                                                    {
                                                      return $hs_int((self["checkValidity"]() ? 1 : 0));
                                                    });
var webkit_dom_html_field_set_element_set_custom_validity;
webkit_dom_html_field_set_element_set_custom_validity = (function(self,
                                                                  error)
                                                         {
                                                           return self["setCustomValidity"]($hs_fromUtf8(error));
                                                         });
var webkit_dom_html_field_set_element_get_form;
webkit_dom_html_field_set_element_get_form = (function(self)
                                              {
                                                return self["form"];
                                              });
var webkit_dom_html_field_set_element_get_validity;
webkit_dom_html_field_set_element_get_validity = (function(self)
                                                  {
                                                    return self["validity"];
                                                  });
var webkit_dom_html_field_set_element_get_will_validate;
webkit_dom_html_field_set_element_get_will_validate = (function(self)
                                                       {
                                                         return $hs_int((self["willValidate"] ? 1 : 0));
                                                       });
var webkit_dom_html_field_set_element_get_validation_message;
webkit_dom_html_field_set_element_get_validation_message = (function(self)
                                                            {
                                                              return $hs_toUtf8(self["validationMessage"]);
                                                            });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_html_element_get_type = (function()
                                    {
                                      return HTMLElement;
                                    });
var webkit_dom_html_element_insert_adjacent_element;
webkit_dom_html_element_insert_adjacent_element = (function(self,
                                                            where, element)
                                                   {
                                                     return self["insertAdjacentElement"]($hs_fromUtf8(where),
                                                                                          element);
                                                   });
var webkit_dom_html_element_insert_adjacent_html;
webkit_dom_html_element_insert_adjacent_html = (function(self,
                                                         where, html)
                                                {
                                                  return self["insertAdjacentHTML"]($hs_fromUtf8(where),
                                                                                    $hs_fromUtf8(html));
                                                });
var webkit_dom_html_element_insert_adjacent_text;
webkit_dom_html_element_insert_adjacent_text = (function(self,
                                                         where, text)
                                                {
                                                  return self["insertAdjacentText"]($hs_fromUtf8(where),
                                                                                    $hs_fromUtf8(text));
                                                });
var webkit_dom_html_element_click;
webkit_dom_html_element_click = (function(self)
                                 {
                                   return self["click"]();
                                 });
var webkit_dom_html_element_set_id;
webkit_dom_html_element_set_id = (function(self, val)
                                  {
                                    self["id"] = $hs_fromUtf8(val);
                                  });
var webkit_dom_html_element_get_id;
webkit_dom_html_element_get_id = (function(self)
                                  {
                                    return $hs_toUtf8(self["id"]);
                                  });
var webkit_dom_html_element_set_title;
webkit_dom_html_element_set_title = (function(self, val)
                                     {
                                       self["title"] = $hs_fromUtf8(val);
                                     });
var webkit_dom_html_element_get_title;
webkit_dom_html_element_get_title = (function(self)
                                     {
                                       return $hs_toUtf8(self["title"]);
                                     });
var webkit_dom_html_element_set_lang;
webkit_dom_html_element_set_lang = (function(self, val)
                                    {
                                      self["lang"] = $hs_fromUtf8(val);
                                    });
var webkit_dom_html_element_get_lang;
webkit_dom_html_element_get_lang = (function(self)
                                    {
                                      return $hs_toUtf8(self["lang"]);
                                    });
var webkit_dom_html_element_set_translate;
webkit_dom_html_element_set_translate = (function(self, val)
                                         {
                                           self["translate"] = ($hs_intToNumber(val) != 0);
                                         });
var webkit_dom_html_element_get_translate;
webkit_dom_html_element_get_translate = (function(self)
                                         {
                                           return $hs_int((self["translate"] ? 1 : 0));
                                         });
var webkit_dom_html_element_set_dir;
webkit_dom_html_element_set_dir = (function(self, val)
                                   {
                                     self["dir"] = $hs_fromUtf8(val);
                                   });
var webkit_dom_html_element_get_dir;
webkit_dom_html_element_get_dir = (function(self)
                                   {
                                     return $hs_toUtf8(self["dir"]);
                                   });
var webkit_dom_html_element_set_class_name;
webkit_dom_html_element_set_class_name = (function(self, val)
                                          {
                                            self["className"] = $hs_fromUtf8(val);
                                          });
var webkit_dom_html_element_get_class_name;
webkit_dom_html_element_get_class_name = (function(self)
                                          {
                                            return $hs_toUtf8(self["className"]);
                                          });
var webkit_dom_html_element_get_class_list;
webkit_dom_html_element_get_class_list = (function(self)
                                          {
                                            return self["classList"];
                                          });
var webkit_dom_html_element_set_tab_index;
webkit_dom_html_element_set_tab_index = (function(self, val)
                                         {
                                           self["tabIndex"] = $hs_intToNumber(val);
                                         });
var webkit_dom_html_element_get_tab_index;
webkit_dom_html_element_get_tab_index = (function(self)
                                         {
                                           return $hs_int(self["tabIndex"]);
                                         });
var webkit_dom_html_element_set_draggable;
webkit_dom_html_element_set_draggable = (function(self, val)
                                         {
                                           self["draggable"] = ($hs_intToNumber(val) != 0);
                                         });
var webkit_dom_html_element_get_draggable;
webkit_dom_html_element_get_draggable = (function(self)
                                         {
                                           return $hs_int((self["draggable"] ? 1 : 0));
                                         });
var webkit_dom_html_element_set_webkitdropzone;
webkit_dom_html_element_set_webkitdropzone = (function(self, val)
                                              {
                                                self["webkitdropzone"] = $hs_fromUtf8(val);
                                              });
var webkit_dom_html_element_get_webkitdropzone;
webkit_dom_html_element_get_webkitdropzone = (function(self)
                                              {
                                                return $hs_toUtf8(self["webkitdropzone"]);
                                              });
var webkit_dom_html_element_set_hidden;
webkit_dom_html_element_set_hidden = (function(self, val)
                                      {
                                        self["hidden"] = ($hs_intToNumber(val) != 0);
                                      });
var webkit_dom_html_element_get_hidden;
webkit_dom_html_element_get_hidden = (function(self)
                                      {
                                        return $hs_int((self["hidden"] ? 1 : 0));
                                      });
var webkit_dom_html_element_set_access_key;
webkit_dom_html_element_set_access_key = (function(self, val)
                                          {
                                            self["accessKey"] = $hs_fromUtf8(val);
                                          });
var webkit_dom_html_element_get_access_key;
webkit_dom_html_element_get_access_key = (function(self)
                                          {
                                            return $hs_toUtf8(self["accessKey"]);
                                          });
var webkit_dom_html_element_set_inner_html;
webkit_dom_html_element_set_inner_html = (function(self, val)
                                          {
                                            self["innerHTML"] = $hs_fromUtf8(val);
                                          });
var webkit_dom_html_element_get_inner_html;
webkit_dom_html_element_get_inner_html = (function(self)
                                          {
                                            return $hs_toUtf8(self["innerHTML"]);
                                          });
var webkit_dom_html_element_set_inner_text;
webkit_dom_html_element_set_inner_text = (function(self, val)
                                          {
                                            self["innerText"] = $hs_fromUtf8(val);
                                          });
var webkit_dom_html_element_get_inner_text;
webkit_dom_html_element_get_inner_text = (function(self)
                                          {
                                            return $hs_toUtf8(self["innerText"]);
                                          });
var webkit_dom_html_element_set_outer_html;
webkit_dom_html_element_set_outer_html = (function(self, val)
                                          {
                                            self["outerHTML"] = $hs_fromUtf8(val);
                                          });
var webkit_dom_html_element_get_outer_html;
webkit_dom_html_element_get_outer_html = (function(self)
                                          {
                                            return $hs_toUtf8(self["outerHTML"]);
                                          });
var webkit_dom_html_element_set_outer_text;
webkit_dom_html_element_set_outer_text = (function(self, val)
                                          {
                                            self["outerText"] = $hs_fromUtf8(val);
                                          });
var webkit_dom_html_element_get_outer_text;
webkit_dom_html_element_get_outer_text = (function(self)
                                          {
                                            return $hs_toUtf8(self["outerText"]);
                                          });
var webkit_dom_html_element_get_children;
webkit_dom_html_element_get_children = (function(self)
                                        {
                                          return self["children"];
                                        });
var webkit_dom_html_element_set_content_editable;
webkit_dom_html_element_set_content_editable = (function(self, val)
                                                {
                                                  self["contentEditable"] = $hs_fromUtf8(val);
                                                });
var webkit_dom_html_element_get_content_editable;
webkit_dom_html_element_get_content_editable = (function(self)
                                                {
                                                  return $hs_toUtf8(self["contentEditable"]);
                                                });
var webkit_dom_html_element_get_is_content_editable;
webkit_dom_html_element_get_is_content_editable = (function(self)
                                                   {
                                                     return $hs_int((self["isContentEditable"] ? 1 : 0));
                                                   });
var webkit_dom_html_element_set_spellcheck;
webkit_dom_html_element_set_spellcheck = (function(self, val)
                                          {
                                            self["spellcheck"] = ($hs_intToNumber(val) != 0);
                                          });
var webkit_dom_html_element_get_spellcheck;
webkit_dom_html_element_get_spellcheck = (function(self)
                                          {
                                            return $hs_int((self["spellcheck"] ? 1 : 0));
                                          });
var webkit_dom_html_element_set_item_scope;
webkit_dom_html_element_set_item_scope = (function(self, val)
                                          {
                                            self["itemScope"] = ($hs_intToNumber(val) != 0);
                                          });
var webkit_dom_html_element_get_item_scope;
webkit_dom_html_element_get_item_scope = (function(self)
                                          {
                                            return $hs_int((self["itemScope"] ? 1 : 0));
                                          });
var webkit_dom_html_element_get_item_type;
webkit_dom_html_element_get_item_type = (function(self)
                                         {
                                           return self["itemType"];
                                         });
var webkit_dom_html_element_set_item_id;
webkit_dom_html_element_set_item_id = (function(self, val)
                                       {
                                         self["itemId"] = $hs_fromUtf8(val);
                                       });
var webkit_dom_html_element_get_item_id;
webkit_dom_html_element_get_item_id = (function(self)
                                       {
                                         return $hs_toUtf8(self["itemId"]);
                                       });
var webkit_dom_html_element_get_item_ref;
webkit_dom_html_element_get_item_ref = (function(self)
                                        {
                                          return self["itemRef"];
                                        });
var webkit_dom_html_element_get_item_prop;
webkit_dom_html_element_get_item_prop = (function(self)
                                         {
                                           return self["itemProp"];
                                         });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_html_document_get_type = (function()
                                     {
                                       return HTMLDocument;
                                     });
var webkit_dom_html_document_open;
webkit_dom_html_document_open = (function(self)
                                 {
                                   return self["open"]();
                                 });
var webkit_dom_html_document_close;
webkit_dom_html_document_close = (function(self)
                                  {
                                    return self["close"]();
                                  });
var webkit_dom_html_document_clear;
webkit_dom_html_document_clear = (function(self)
                                  {
                                    return self["clear"]();
                                  });
var webkit_dom_html_document_capture_events;
webkit_dom_html_document_capture_events = (function(self)
                                           {
                                             return self["captureEvents"]();
                                           });
var webkit_dom_html_document_release_events;
webkit_dom_html_document_release_events = (function(self)
                                           {
                                             return self["releaseEvents"]();
                                           });
var webkit_dom_html_document_has_focus;
webkit_dom_html_document_has_focus = (function(self)
                                      {
                                        return $hs_int((self["hasFocus"]() ? 1 : 0));
                                      });
var webkit_dom_html_document_get_embeds;
webkit_dom_html_document_get_embeds = (function(self)
                                       {
                                         return self["embeds"];
                                       });
var webkit_dom_html_document_get_plugins;
webkit_dom_html_document_get_plugins = (function(self)
                                        {
                                          return self["plugins"];
                                        });
var webkit_dom_html_document_get_scripts;
webkit_dom_html_document_get_scripts = (function(self)
                                        {
                                          return self["scripts"];
                                        });
var webkit_dom_html_document_get_width;
webkit_dom_html_document_get_width = (function(self)
                                      {
                                        return $hs_int(self["width"]);
                                      });
var webkit_dom_html_document_get_height;
webkit_dom_html_document_get_height = (function(self)
                                       {
                                         return $hs_int(self["height"]);
                                       });
var webkit_dom_html_document_set_dir;
webkit_dom_html_document_set_dir = (function(self, val)
                                    {
                                      self["dir"] = $hs_fromUtf8(val);
                                    });
var webkit_dom_html_document_get_dir;
webkit_dom_html_document_get_dir = (function(self)
                                    {
                                      return $hs_toUtf8(self["dir"]);
                                    });
var webkit_dom_html_document_set_design_mode;
webkit_dom_html_document_set_design_mode = (function(self, val)
                                            {
                                              self["designMode"] = $hs_fromUtf8(val);
                                            });
var webkit_dom_html_document_get_design_mode;
webkit_dom_html_document_get_design_mode = (function(self)
                                            {
                                              return $hs_toUtf8(self["designMode"]);
                                            });
var webkit_dom_html_document_get_compat_mode;
webkit_dom_html_document_get_compat_mode = (function(self)
                                            {
                                              return $hs_toUtf8(self["compatMode"]);
                                            });
var webkit_dom_html_document_get_active_element;
webkit_dom_html_document_get_active_element = (function(self)
                                               {
                                                 return self["activeElement"];
                                               });
var webkit_dom_html_document_set_bg_color;
webkit_dom_html_document_set_bg_color = (function(self, val)
                                         {
                                           self["bgColor"] = $hs_fromUtf8(val);
                                         });
var webkit_dom_html_document_get_bg_color;
webkit_dom_html_document_get_bg_color = (function(self)
                                         {
                                           return $hs_toUtf8(self["bgColor"]);
                                         });
var webkit_dom_html_document_set_fg_color;
webkit_dom_html_document_set_fg_color = (function(self, val)
                                         {
                                           self["fgColor"] = $hs_fromUtf8(val);
                                         });
var webkit_dom_html_document_get_fg_color;
webkit_dom_html_document_get_fg_color = (function(self)
                                         {
                                           return $hs_toUtf8(self["fgColor"]);
                                         });
var webkit_dom_html_document_set_alink_color;
webkit_dom_html_document_set_alink_color = (function(self, val)
                                            {
                                              self["alinkColor"] = $hs_fromUtf8(val);
                                            });
var webkit_dom_html_document_get_alink_color;
webkit_dom_html_document_get_alink_color = (function(self)
                                            {
                                              return $hs_toUtf8(self["alinkColor"]);
                                            });
var webkit_dom_html_document_set_link_color;
webkit_dom_html_document_set_link_color = (function(self, val)
                                           {
                                             self["linkColor"] = $hs_fromUtf8(val);
                                           });
var webkit_dom_html_document_get_link_color;
webkit_dom_html_document_get_link_color = (function(self)
                                           {
                                             return $hs_toUtf8(self["linkColor"]);
                                           });
var webkit_dom_html_document_set_vlink_color;
webkit_dom_html_document_set_vlink_color = (function(self, val)
                                            {
                                              self["vlinkColor"] = $hs_fromUtf8(val);
                                            });
var webkit_dom_html_document_get_vlink_color;
webkit_dom_html_document_get_vlink_color = (function(self)
                                            {
                                              return $hs_toUtf8(self["vlinkColor"]);
                                            });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_htmld_list_element_get_type = (function()
                                          {
                                            return HTMLDListElement;
                                          });
var webkit_dom_htmld_list_element_set_compact;
webkit_dom_htmld_list_element_set_compact = (function(self, val)
                                             {
                                               self["compact"] = ($hs_intToNumber(val) != 0);
                                             });
var webkit_dom_htmld_list_element_get_compact;
webkit_dom_htmld_list_element_get_compact = (function(self)
                                             {
                                               return $hs_int((self["compact"] ? 1 : 0));
                                             });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_html_div_element_get_type = (function()
                                        {
                                          return HTMLDivElement;
                                        });
var webkit_dom_html_div_element_set_align;
webkit_dom_html_div_element_set_align = (function(self, val)
                                         {
                                           self["align"] = $hs_fromUtf8(val);
                                         });
var webkit_dom_html_div_element_get_align;
webkit_dom_html_div_element_get_align = (function(self)
                                         {
                                           return $hs_toUtf8(self["align"]);
                                         });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_html_directory_element_get_type = (function()
                                              {
                                                return HTMLDirectoryElement;
                                              });
var webkit_dom_html_directory_element_set_compact;
webkit_dom_html_directory_element_set_compact = (function(self,
                                                          val)
                                                 {
                                                   self["compact"] = ($hs_intToNumber(val) != 0);
                                                 });
var webkit_dom_html_directory_element_get_compact;
webkit_dom_html_directory_element_get_compact = (function(self)
                                                 {
                                                   return $hs_int((self["compact"] ? 1 : 0));
                                                 });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_html_collection_get_type = (function()
                                       {
                                         return HTMLCollection;
                                       });
var webkit_dom_html_collection_item;
webkit_dom_html_collection_item = (function(self, index)
                                   {
                                     return self["item"]($hs_intToNumber(index));
                                   });
var webkit_dom_html_collection_get_length;
webkit_dom_html_collection_get_length = (function(self)
                                         {
                                           return $hs_int(self["length"]);
                                         });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_html_button_element_get_type = (function()
                                           {
                                             return HTMLButtonElement;
                                           });
var webkit_dom_html_button_element_check_validity;
webkit_dom_html_button_element_check_validity = (function(self)
                                                 {
                                                   return $hs_int((self["checkValidity"]() ? 1 : 0));
                                                 });
var webkit_dom_html_button_element_set_custom_validity;
webkit_dom_html_button_element_set_custom_validity = (function(self,
                                                               error)
                                                      {
                                                        return self["setCustomValidity"]($hs_fromUtf8(error));
                                                      });
var webkit_dom_html_button_element_get_form;
webkit_dom_html_button_element_get_form = (function(self)
                                           {
                                             return self["form"];
                                           });
var webkit_dom_html_button_element_set_form_action;
webkit_dom_html_button_element_set_form_action = (function(self,
                                                           val)
                                                  {
                                                    self["formAction"] = $hs_fromUtf8(val);
                                                  });
var webkit_dom_html_button_element_get_form_action;
webkit_dom_html_button_element_get_form_action = (function(self)
                                                  {
                                                    return $hs_toUtf8(self["formAction"]);
                                                  });
var webkit_dom_html_button_element_set_form_enctype;
webkit_dom_html_button_element_set_form_enctype = (function(self,
                                                            val)
                                                   {
                                                     self["formEnctype"] = $hs_fromUtf8(val);
                                                   });
var webkit_dom_html_button_element_get_form_enctype;
webkit_dom_html_button_element_get_form_enctype = (function(self)
                                                   {
                                                     return $hs_toUtf8(self["formEnctype"]);
                                                   });
var webkit_dom_html_button_element_set_form_method;
webkit_dom_html_button_element_set_form_method = (function(self,
                                                           val)
                                                  {
                                                    self["formMethod"] = $hs_fromUtf8(val);
                                                  });
var webkit_dom_html_button_element_get_form_method;
webkit_dom_html_button_element_get_form_method = (function(self)
                                                  {
                                                    return $hs_toUtf8(self["formMethod"]);
                                                  });
var webkit_dom_html_button_element_set_form_no_validate;
webkit_dom_html_button_element_set_form_no_validate = (function(self,
                                                                val)
                                                       {
                                                         self["formNoValidate"] = ($hs_intToNumber(val)
                                                                                   !=
                                                                                   0);
                                                       });
var webkit_dom_html_button_element_get_form_no_validate;
webkit_dom_html_button_element_get_form_no_validate = (function(self)
                                                       {
                                                         return $hs_int((self["formNoValidate"] ? 1 : 0));
                                                       });
var webkit_dom_html_button_element_set_form_target;
webkit_dom_html_button_element_set_form_target = (function(self,
                                                           val)
                                                  {
                                                    self["formTarget"] = $hs_fromUtf8(val);
                                                  });
var webkit_dom_html_button_element_get_form_target;
webkit_dom_html_button_element_get_form_target = (function(self)
                                                  {
                                                    return $hs_toUtf8(self["formTarget"]);
                                                  });
var webkit_dom_html_button_element_get_validity;
webkit_dom_html_button_element_get_validity = (function(self)
                                               {
                                                 return self["validity"];
                                               });
var webkit_dom_html_button_element_set_disabled;
webkit_dom_html_button_element_set_disabled = (function(self, val)
                                               {
                                                 self["disabled"] = ($hs_intToNumber(val) != 0);
                                               });
var webkit_dom_html_button_element_get_disabled;
webkit_dom_html_button_element_get_disabled = (function(self)
                                               {
                                                 return $hs_int((self["disabled"] ? 1 : 0));
                                               });
var webkit_dom_html_button_element_set_autofocus;
webkit_dom_html_button_element_set_autofocus = (function(self, val)
                                                {
                                                  self["autofocus"] = ($hs_intToNumber(val) != 0);
                                                });
var webkit_dom_html_button_element_get_autofocus;
webkit_dom_html_button_element_get_autofocus = (function(self)
                                                {
                                                  return $hs_int((self["autofocus"] ? 1 : 0));
                                                });
var webkit_dom_html_button_element_set_name;
webkit_dom_html_button_element_set_name = (function(self, val)
                                           {
                                             self["name"] = $hs_fromUtf8(val);
                                           });
var webkit_dom_html_button_element_get_name;
webkit_dom_html_button_element_get_name = (function(self)
                                           {
                                             return $hs_toUtf8(self["name"]);
                                           });
var webkit_dom_html_button_element_set_value;
webkit_dom_html_button_element_set_value = (function(self, val)
                                            {
                                              self["value"] = $hs_fromUtf8(val);
                                            });
var webkit_dom_html_button_element_get_value;
webkit_dom_html_button_element_get_value = (function(self)
                                            {
                                              return $hs_toUtf8(self["value"]);
                                            });
var webkit_dom_html_button_element_get_will_validate;
webkit_dom_html_button_element_get_will_validate = (function(self)
                                                    {
                                                      return $hs_int((self["willValidate"] ? 1 : 0));
                                                    });
var webkit_dom_html_button_element_get_validation_message;
webkit_dom_html_button_element_get_validation_message = (function(self)
                                                         {
                                                           return $hs_toUtf8(self["validationMessage"]);
                                                         });
var webkit_dom_html_button_element_get_labels;
webkit_dom_html_button_element_get_labels = (function(self)
                                             {
                                               return self["labels"];
                                             });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_htmlbr_element_get_type = (function()
                                      {
                                        return HTMLBRElement;
                                      });
var webkit_dom_htmlbr_element_set_clear;
webkit_dom_htmlbr_element_set_clear = (function(self, val)
                                       {
                                         self["clear"] = $hs_fromUtf8(val);
                                       });
var webkit_dom_htmlbr_element_get_clear;
webkit_dom_htmlbr_element_get_clear = (function(self)
                                       {
                                         return $hs_toUtf8(self["clear"]);
                                       });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_html_body_element_get_type = (function()
                                         {
                                           return HTMLBodyElement;
                                         });
var webkit_dom_html_body_element_set_a_link;
webkit_dom_html_body_element_set_a_link = (function(self, val)
                                           {
                                             self["aLink"] = $hs_fromUtf8(val);
                                           });
var webkit_dom_html_body_element_get_a_link;
webkit_dom_html_body_element_get_a_link = (function(self)
                                           {
                                             return $hs_toUtf8(self["aLink"]);
                                           });
var webkit_dom_html_body_element_set_background;
webkit_dom_html_body_element_set_background = (function(self, val)
                                               {
                                                 self["background"] = $hs_fromUtf8(val);
                                               });
var webkit_dom_html_body_element_get_background;
webkit_dom_html_body_element_get_background = (function(self)
                                               {
                                                 return $hs_toUtf8(self["background"]);
                                               });
var webkit_dom_html_body_element_set_bg_color;
webkit_dom_html_body_element_set_bg_color = (function(self, val)
                                             {
                                               self["bgColor"] = $hs_fromUtf8(val);
                                             });
var webkit_dom_html_body_element_get_bg_color;
webkit_dom_html_body_element_get_bg_color = (function(self)
                                             {
                                               return $hs_toUtf8(self["bgColor"]);
                                             });
var webkit_dom_html_body_element_set_link;
webkit_dom_html_body_element_set_link = (function(self, val)
                                         {
                                           self["link"] = $hs_fromUtf8(val);
                                         });
var webkit_dom_html_body_element_get_link;
webkit_dom_html_body_element_get_link = (function(self)
                                         {
                                           return $hs_toUtf8(self["link"]);
                                         });
var webkit_dom_html_body_element_set_text;
webkit_dom_html_body_element_set_text = (function(self, val)
                                         {
                                           self["text"] = $hs_fromUtf8(val);
                                         });
var webkit_dom_html_body_element_get_text;
webkit_dom_html_body_element_get_text = (function(self)
                                         {
                                           return $hs_toUtf8(self["text"]);
                                         });
var webkit_dom_html_body_element_set_v_link;
webkit_dom_html_body_element_set_v_link = (function(self, val)
                                           {
                                             self["vLink"] = $hs_fromUtf8(val);
                                           });
var webkit_dom_html_body_element_get_v_link;
webkit_dom_html_body_element_get_v_link = (function(self)
                                           {
                                             return $hs_toUtf8(self["vLink"]);
                                           });
var webkit_dom_html_body_element_set_onbeforeunload;
webkit_dom_html_body_element_set_onbeforeunload = (function(self,
                                                            val)
                                                   {
                                                     self["onbeforeunload"] = val;
                                                   });
var webkit_dom_html_body_element_get_onbeforeunload;
webkit_dom_html_body_element_get_onbeforeunload = (function(self)
                                                   {
                                                     return self["onbeforeunload"];
                                                   });
var webkit_dom_html_body_element_set_onhashchange;
webkit_dom_html_body_element_set_onhashchange = (function(self,
                                                          val)
                                                 {
                                                   self["onhashchange"] = val;
                                                 });
var webkit_dom_html_body_element_get_onhashchange;
webkit_dom_html_body_element_get_onhashchange = (function(self)
                                                 {
                                                   return self["onhashchange"];
                                                 });
var webkit_dom_html_body_element_set_onmessage;
webkit_dom_html_body_element_set_onmessage = (function(self, val)
                                              {
                                                self["onmessage"] = val;
                                              });
var webkit_dom_html_body_element_get_onmessage;
webkit_dom_html_body_element_get_onmessage = (function(self)
                                              {
                                                return self["onmessage"];
                                              });
var webkit_dom_html_body_element_set_onoffline;
webkit_dom_html_body_element_set_onoffline = (function(self, val)
                                              {
                                                self["onoffline"] = val;
                                              });
var webkit_dom_html_body_element_get_onoffline;
webkit_dom_html_body_element_get_onoffline = (function(self)
                                              {
                                                return self["onoffline"];
                                              });
var webkit_dom_html_body_element_set_ononline;
webkit_dom_html_body_element_set_ononline = (function(self, val)
                                             {
                                               self["ononline"] = val;
                                             });
var webkit_dom_html_body_element_get_ononline;
webkit_dom_html_body_element_get_ononline = (function(self)
                                             {
                                               return self["ononline"];
                                             });
var webkit_dom_html_body_element_set_onpopstate;
webkit_dom_html_body_element_set_onpopstate = (function(self, val)
                                               {
                                                 self["onpopstate"] = val;
                                               });
var webkit_dom_html_body_element_get_onpopstate;
webkit_dom_html_body_element_get_onpopstate = (function(self)
                                               {
                                                 return self["onpopstate"];
                                               });
var webkit_dom_html_body_element_set_onresize;
webkit_dom_html_body_element_set_onresize = (function(self, val)
                                             {
                                               self["onresize"] = val;
                                             });
var webkit_dom_html_body_element_get_onresize;
webkit_dom_html_body_element_get_onresize = (function(self)
                                             {
                                               return self["onresize"];
                                             });
var webkit_dom_html_body_element_set_onstorage;
webkit_dom_html_body_element_set_onstorage = (function(self, val)
                                              {
                                                self["onstorage"] = val;
                                              });
var webkit_dom_html_body_element_get_onstorage;
webkit_dom_html_body_element_get_onstorage = (function(self)
                                              {
                                                return self["onstorage"];
                                              });
var webkit_dom_html_body_element_set_onunload;
webkit_dom_html_body_element_set_onunload = (function(self, val)
                                             {
                                               self["onunload"] = val;
                                             });
var webkit_dom_html_body_element_get_onunload;
webkit_dom_html_body_element_get_onunload = (function(self)
                                             {
                                               return self["onunload"];
                                             });
var webkit_dom_html_body_element_set_onorientationchange;
webkit_dom_html_body_element_set_onorientationchange = (function(self,
                                                                 val)
                                                        {
                                                          self["onorientationchange"] = val;
                                                        });
var webkit_dom_html_body_element_get_onorientationchange;
webkit_dom_html_body_element_get_onorientationchange = (function(self)
                                                        {
                                                          return self["onorientationchange"];
                                                        });
var webkit_dom_html_body_element_set_onblur;
webkit_dom_html_body_element_set_onblur = (function(self, val)
                                           {
                                             self["onblur"] = val;
                                           });
var webkit_dom_html_body_element_get_onblur;
webkit_dom_html_body_element_get_onblur = (function(self)
                                           {
                                             return self["onblur"];
                                           });
var webkit_dom_html_body_element_set_onerror;
webkit_dom_html_body_element_set_onerror = (function(self, val)
                                            {
                                              self["onerror"] = val;
                                            });
var webkit_dom_html_body_element_get_onerror;
webkit_dom_html_body_element_get_onerror = (function(self)
                                            {
                                              return self["onerror"];
                                            });
var webkit_dom_html_body_element_set_onfocus;
webkit_dom_html_body_element_set_onfocus = (function(self, val)
                                            {
                                              self["onfocus"] = val;
                                            });
var webkit_dom_html_body_element_get_onfocus;
webkit_dom_html_body_element_get_onfocus = (function(self)
                                            {
                                              return self["onfocus"];
                                            });
var webkit_dom_html_body_element_set_onload;
webkit_dom_html_body_element_set_onload = (function(self, val)
                                           {
                                             self["onload"] = val;
                                           });
var webkit_dom_html_body_element_get_onload;
webkit_dom_html_body_element_get_onload = (function(self)
                                           {
                                             return self["onload"];
                                           });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_html_base_font_element_get_type = (function()
                                              {
                                                return HTMLBaseFontElement;
                                              });
var webkit_dom_html_base_font_element_set_color;
webkit_dom_html_base_font_element_set_color = (function(self, val)
                                               {
                                                 self["color"] = $hs_fromUtf8(val);
                                               });
var webkit_dom_html_base_font_element_get_color;
webkit_dom_html_base_font_element_get_color = (function(self)
                                               {
                                                 return $hs_toUtf8(self["color"]);
                                               });
var webkit_dom_html_base_font_element_set_face;
webkit_dom_html_base_font_element_set_face = (function(self, val)
                                              {
                                                self["face"] = $hs_fromUtf8(val);
                                              });
var webkit_dom_html_base_font_element_get_face;
webkit_dom_html_base_font_element_get_face = (function(self)
                                              {
                                                return $hs_toUtf8(self["face"]);
                                              });
var webkit_dom_html_base_font_element_set_size;
webkit_dom_html_base_font_element_set_size = (function(self, val)
                                              {
                                                self["size"] = $hs_intToNumber(val);
                                              });
var webkit_dom_html_base_font_element_get_size;
webkit_dom_html_base_font_element_get_size = (function(self)
                                              {
                                                return $hs_int(self["size"]);
                                              });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_html_base_element_get_type = (function()
                                         {
                                           return HTMLBaseElement;
                                         });
var webkit_dom_html_base_element_set_href;
webkit_dom_html_base_element_set_href = (function(self, val)
                                         {
                                           self["href"] = $hs_fromUtf8(val);
                                         });
var webkit_dom_html_base_element_get_href;
webkit_dom_html_base_element_get_href = (function(self)
                                         {
                                           return $hs_toUtf8(self["href"]);
                                         });
var webkit_dom_html_base_element_set_target;
webkit_dom_html_base_element_set_target = (function(self, val)
                                           {
                                             self["target"] = $hs_fromUtf8(val);
                                           });
var webkit_dom_html_base_element_get_target;
webkit_dom_html_base_element_get_target = (function(self)
                                           {
                                             return $hs_toUtf8(self["target"]);
                                           });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_html_area_element_get_type = (function()
                                         {
                                           return HTMLAreaElement;
                                         });
var webkit_dom_html_area_element_set_alt;
webkit_dom_html_area_element_set_alt = (function(self, val)
                                        {
                                          self["alt"] = $hs_fromUtf8(val);
                                        });
var webkit_dom_html_area_element_get_alt;
webkit_dom_html_area_element_get_alt = (function(self)
                                        {
                                          return $hs_toUtf8(self["alt"]);
                                        });
var webkit_dom_html_area_element_set_coords;
webkit_dom_html_area_element_set_coords = (function(self, val)
                                           {
                                             self["coords"] = $hs_fromUtf8(val);
                                           });
var webkit_dom_html_area_element_get_coords;
webkit_dom_html_area_element_get_coords = (function(self)
                                           {
                                             return $hs_toUtf8(self["coords"]);
                                           });
var webkit_dom_html_area_element_set_href;
webkit_dom_html_area_element_set_href = (function(self, val)
                                         {
                                           self["href"] = $hs_fromUtf8(val);
                                         });
var webkit_dom_html_area_element_get_href;
webkit_dom_html_area_element_get_href = (function(self)
                                         {
                                           return $hs_toUtf8(self["href"]);
                                         });
var webkit_dom_html_area_element_set_no_href;
webkit_dom_html_area_element_set_no_href = (function(self, val)
                                            {
                                              self["noHref"] = ($hs_intToNumber(val) != 0);
                                            });
var webkit_dom_html_area_element_get_no_href;
webkit_dom_html_area_element_get_no_href = (function(self)
                                            {
                                              return $hs_int((self["noHref"] ? 1 : 0));
                                            });
var webkit_dom_html_area_element_set_ping;
webkit_dom_html_area_element_set_ping = (function(self, val)
                                         {
                                           self["ping"] = $hs_fromUtf8(val);
                                         });
var webkit_dom_html_area_element_get_ping;
webkit_dom_html_area_element_get_ping = (function(self)
                                         {
                                           return $hs_toUtf8(self["ping"]);
                                         });
var webkit_dom_html_area_element_set_shape;
webkit_dom_html_area_element_set_shape = (function(self, val)
                                          {
                                            self["shape"] = $hs_fromUtf8(val);
                                          });
var webkit_dom_html_area_element_get_shape;
webkit_dom_html_area_element_get_shape = (function(self)
                                          {
                                            return $hs_toUtf8(self["shape"]);
                                          });
var webkit_dom_html_area_element_set_target;
webkit_dom_html_area_element_set_target = (function(self, val)
                                           {
                                             self["target"] = $hs_fromUtf8(val);
                                           });
var webkit_dom_html_area_element_get_target;
webkit_dom_html_area_element_get_target = (function(self)
                                           {
                                             return $hs_toUtf8(self["target"]);
                                           });
var webkit_dom_html_area_element_get_hash;
webkit_dom_html_area_element_get_hash = (function(self)
                                         {
                                           return $hs_toUtf8(self["hash"]);
                                         });
var webkit_dom_html_area_element_get_host;
webkit_dom_html_area_element_get_host = (function(self)
                                         {
                                           return $hs_toUtf8(self["host"]);
                                         });
var webkit_dom_html_area_element_get_hostname;
webkit_dom_html_area_element_get_hostname = (function(self)
                                             {
                                               return $hs_toUtf8(self["hostname"]);
                                             });
var webkit_dom_html_area_element_get_pathname;
webkit_dom_html_area_element_get_pathname = (function(self)
                                             {
                                               return $hs_toUtf8(self["pathname"]);
                                             });
var webkit_dom_html_area_element_get_port;
webkit_dom_html_area_element_get_port = (function(self)
                                         {
                                           return $hs_toUtf8(self["port"]);
                                         });
var webkit_dom_html_area_element_get_protocol;
webkit_dom_html_area_element_get_protocol = (function(self)
                                             {
                                               return $hs_toUtf8(self["protocol"]);
                                             });
var webkit_dom_html_area_element_get_search;
webkit_dom_html_area_element_get_search = (function(self)
                                           {
                                             return $hs_toUtf8(self["search"]);
                                           });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_html_applet_element_get_type = (function()
                                           {
                                             return HTMLAppletElement;
                                           });
var webkit_dom_html_applet_element_set_align;
webkit_dom_html_applet_element_set_align = (function(self, val)
                                            {
                                              self["align"] = $hs_fromUtf8(val);
                                            });
var webkit_dom_html_applet_element_get_align;
webkit_dom_html_applet_element_get_align = (function(self)
                                            {
                                              return $hs_toUtf8(self["align"]);
                                            });
var webkit_dom_html_applet_element_set_alt;
webkit_dom_html_applet_element_set_alt = (function(self, val)
                                          {
                                            self["alt"] = $hs_fromUtf8(val);
                                          });
var webkit_dom_html_applet_element_get_alt;
webkit_dom_html_applet_element_get_alt = (function(self)
                                          {
                                            return $hs_toUtf8(self["alt"]);
                                          });
var webkit_dom_html_applet_element_set_archive;
webkit_dom_html_applet_element_set_archive = (function(self, val)
                                              {
                                                self["archive"] = $hs_fromUtf8(val);
                                              });
var webkit_dom_html_applet_element_get_archive;
webkit_dom_html_applet_element_get_archive = (function(self)
                                              {
                                                return $hs_toUtf8(self["archive"]);
                                              });
var webkit_dom_html_applet_element_set_code;
webkit_dom_html_applet_element_set_code = (function(self, val)
                                           {
                                             self["code"] = $hs_fromUtf8(val);
                                           });
var webkit_dom_html_applet_element_get_code;
webkit_dom_html_applet_element_get_code = (function(self)
                                           {
                                             return $hs_toUtf8(self["code"]);
                                           });
var webkit_dom_html_applet_element_set_code_base;
webkit_dom_html_applet_element_set_code_base = (function(self, val)
                                                {
                                                  self["codeBase"] = $hs_fromUtf8(val);
                                                });
var webkit_dom_html_applet_element_get_code_base;
webkit_dom_html_applet_element_get_code_base = (function(self)
                                                {
                                                  return $hs_toUtf8(self["codeBase"]);
                                                });
var webkit_dom_html_applet_element_set_height;
webkit_dom_html_applet_element_set_height = (function(self, val)
                                             {
                                               self["height"] = $hs_fromUtf8(val);
                                             });
var webkit_dom_html_applet_element_get_height;
webkit_dom_html_applet_element_get_height = (function(self)
                                             {
                                               return $hs_toUtf8(self["height"]);
                                             });
var webkit_dom_html_applet_element_set_hspace;
webkit_dom_html_applet_element_set_hspace = (function(self, val)
                                             {
                                               self["hspace"] = $hs_intToNumber(val);
                                             });
var webkit_dom_html_applet_element_get_hspace;
webkit_dom_html_applet_element_get_hspace = (function(self)
                                             {
                                               return $hs_int(self["hspace"]);
                                             });
var webkit_dom_html_applet_element_set_name;
webkit_dom_html_applet_element_set_name = (function(self, val)
                                           {
                                             self["name"] = $hs_fromUtf8(val);
                                           });
var webkit_dom_html_applet_element_get_name;
webkit_dom_html_applet_element_get_name = (function(self)
                                           {
                                             return $hs_toUtf8(self["name"]);
                                           });
var webkit_dom_html_applet_element_set_object;
webkit_dom_html_applet_element_set_object = (function(self, val)
                                             {
                                               self["object"] = $hs_fromUtf8(val);
                                             });
var webkit_dom_html_applet_element_get_object;
webkit_dom_html_applet_element_get_object = (function(self)
                                             {
                                               return $hs_toUtf8(self["object"]);
                                             });
var webkit_dom_html_applet_element_set_vspace;
webkit_dom_html_applet_element_set_vspace = (function(self, val)
                                             {
                                               self["vspace"] = $hs_intToNumber(val);
                                             });
var webkit_dom_html_applet_element_get_vspace;
webkit_dom_html_applet_element_get_vspace = (function(self)
                                             {
                                               return $hs_int(self["vspace"]);
                                             });
var webkit_dom_html_applet_element_set_width;
webkit_dom_html_applet_element_set_width = (function(self, val)
                                            {
                                              self["width"] = $hs_fromUtf8(val);
                                            });
var webkit_dom_html_applet_element_get_width;
webkit_dom_html_applet_element_get_width = (function(self)
                                            {
                                              return $hs_toUtf8(self["width"]);
                                            });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_html_anchor_element_get_type = (function()
                                           {
                                             return HTMLAnchorElement;
                                           });
var webkit_dom_html_anchor_element_set_charset;
webkit_dom_html_anchor_element_set_charset = (function(self, val)
                                              {
                                                self["charset"] = $hs_fromUtf8(val);
                                              });
var webkit_dom_html_anchor_element_get_charset;
webkit_dom_html_anchor_element_get_charset = (function(self)
                                              {
                                                return $hs_toUtf8(self["charset"]);
                                              });
var webkit_dom_html_anchor_element_set_coords;
webkit_dom_html_anchor_element_set_coords = (function(self, val)
                                             {
                                               self["coords"] = $hs_fromUtf8(val);
                                             });
var webkit_dom_html_anchor_element_get_coords;
webkit_dom_html_anchor_element_get_coords = (function(self)
                                             {
                                               return $hs_toUtf8(self["coords"]);
                                             });
var webkit_dom_html_anchor_element_set_download;
webkit_dom_html_anchor_element_set_download = (function(self, val)
                                               {
                                                 self["download"] = $hs_fromUtf8(val);
                                               });
var webkit_dom_html_anchor_element_get_download;
webkit_dom_html_anchor_element_get_download = (function(self)
                                               {
                                                 return $hs_toUtf8(self["download"]);
                                               });
var webkit_dom_html_anchor_element_set_href;
webkit_dom_html_anchor_element_set_href = (function(self, val)
                                           {
                                             self["href"] = $hs_fromUtf8(val);
                                           });
var webkit_dom_html_anchor_element_get_href;
webkit_dom_html_anchor_element_get_href = (function(self)
                                           {
                                             return $hs_toUtf8(self["href"]);
                                           });
var webkit_dom_html_anchor_element_set_hreflang;
webkit_dom_html_anchor_element_set_hreflang = (function(self, val)
                                               {
                                                 self["hreflang"] = $hs_fromUtf8(val);
                                               });
var webkit_dom_html_anchor_element_get_hreflang;
webkit_dom_html_anchor_element_get_hreflang = (function(self)
                                               {
                                                 return $hs_toUtf8(self["hreflang"]);
                                               });
var webkit_dom_html_anchor_element_set_name;
webkit_dom_html_anchor_element_set_name = (function(self, val)
                                           {
                                             self["name"] = $hs_fromUtf8(val);
                                           });
var webkit_dom_html_anchor_element_get_name;
webkit_dom_html_anchor_element_get_name = (function(self)
                                           {
                                             return $hs_toUtf8(self["name"]);
                                           });
var webkit_dom_html_anchor_element_set_ping;
webkit_dom_html_anchor_element_set_ping = (function(self, val)
                                           {
                                             self["ping"] = $hs_fromUtf8(val);
                                           });
var webkit_dom_html_anchor_element_get_ping;
webkit_dom_html_anchor_element_get_ping = (function(self)
                                           {
                                             return $hs_toUtf8(self["ping"]);
                                           });
var webkit_dom_html_anchor_element_set_rel;
webkit_dom_html_anchor_element_set_rel = (function(self, val)
                                          {
                                            self["rel"] = $hs_fromUtf8(val);
                                          });
var webkit_dom_html_anchor_element_get_rel;
webkit_dom_html_anchor_element_get_rel = (function(self)
                                          {
                                            return $hs_toUtf8(self["rel"]);
                                          });
var webkit_dom_html_anchor_element_set_rev;
webkit_dom_html_anchor_element_set_rev = (function(self, val)
                                          {
                                            self["rev"] = $hs_fromUtf8(val);
                                          });
var webkit_dom_html_anchor_element_get_rev;
webkit_dom_html_anchor_element_get_rev = (function(self)
                                          {
                                            return $hs_toUtf8(self["rev"]);
                                          });
var webkit_dom_html_anchor_element_set_shape;
webkit_dom_html_anchor_element_set_shape = (function(self, val)
                                            {
                                              self["shape"] = $hs_fromUtf8(val);
                                            });
var webkit_dom_html_anchor_element_get_shape;
webkit_dom_html_anchor_element_get_shape = (function(self)
                                            {
                                              return $hs_toUtf8(self["shape"]);
                                            });
var webkit_dom_html_anchor_element_set_target;
webkit_dom_html_anchor_element_set_target = (function(self, val)
                                             {
                                               self["target"] = $hs_fromUtf8(val);
                                             });
var webkit_dom_html_anchor_element_get_target;
webkit_dom_html_anchor_element_get_target = (function(self)
                                             {
                                               return $hs_toUtf8(self["target"]);
                                             });
var webkit_dom_html_anchor_element_set_hash;
webkit_dom_html_anchor_element_set_hash = (function(self, val)
                                           {
                                             self["hash"] = $hs_fromUtf8(val);
                                           });
var webkit_dom_html_anchor_element_get_hash;
webkit_dom_html_anchor_element_get_hash = (function(self)
                                           {
                                             return $hs_toUtf8(self["hash"]);
                                           });
var webkit_dom_html_anchor_element_set_host;
webkit_dom_html_anchor_element_set_host = (function(self, val)
                                           {
                                             self["host"] = $hs_fromUtf8(val);
                                           });
var webkit_dom_html_anchor_element_get_host;
webkit_dom_html_anchor_element_get_host = (function(self)
                                           {
                                             return $hs_toUtf8(self["host"]);
                                           });
var webkit_dom_html_anchor_element_set_hostname;
webkit_dom_html_anchor_element_set_hostname = (function(self, val)
                                               {
                                                 self["hostname"] = $hs_fromUtf8(val);
                                               });
var webkit_dom_html_anchor_element_get_hostname;
webkit_dom_html_anchor_element_get_hostname = (function(self)
                                               {
                                                 return $hs_toUtf8(self["hostname"]);
                                               });
var webkit_dom_html_anchor_element_set_pathname;
webkit_dom_html_anchor_element_set_pathname = (function(self, val)
                                               {
                                                 self["pathname"] = $hs_fromUtf8(val);
                                               });
var webkit_dom_html_anchor_element_get_pathname;
webkit_dom_html_anchor_element_get_pathname = (function(self)
                                               {
                                                 return $hs_toUtf8(self["pathname"]);
                                               });
var webkit_dom_html_anchor_element_set_port;
webkit_dom_html_anchor_element_set_port = (function(self, val)
                                           {
                                             self["port"] = $hs_fromUtf8(val);
                                           });
var webkit_dom_html_anchor_element_get_port;
webkit_dom_html_anchor_element_get_port = (function(self)
                                           {
                                             return $hs_toUtf8(self["port"]);
                                           });
var webkit_dom_html_anchor_element_set_protocol;
webkit_dom_html_anchor_element_set_protocol = (function(self, val)
                                               {
                                                 self["protocol"] = $hs_fromUtf8(val);
                                               });
var webkit_dom_html_anchor_element_get_protocol;
webkit_dom_html_anchor_element_get_protocol = (function(self)
                                               {
                                                 return $hs_toUtf8(self["protocol"]);
                                               });
var webkit_dom_html_anchor_element_set_search;
webkit_dom_html_anchor_element_set_search = (function(self, val)
                                             {
                                               self["search"] = $hs_fromUtf8(val);
                                             });
var webkit_dom_html_anchor_element_get_search;
webkit_dom_html_anchor_element_get_search = (function(self)
                                             {
                                               return $hs_toUtf8(self["search"]);
                                             });
var webkit_dom_html_anchor_element_get_origin;
webkit_dom_html_anchor_element_get_origin = (function(self)
                                             {
                                               return $hs_toUtf8(self["origin"]);
                                             });
var webkit_dom_html_anchor_element_get_text;
webkit_dom_html_anchor_element_get_text = (function(self)
                                           {
                                             return $hs_toUtf8(self["text"]);
                                           });
// Graphics.UI.Gtk.WebKit.DOM.Window
webkit_dom_history_get_type = (function()
                               {
                                 return History;
                               });
var webkit_dom_history_get_length;
webkit_dom_history_get_length = (function(self)
                                 {
                                   return $hs_int(self["length"]);
                                 });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_file_list_get_type = (function()
                                 {
                                   return FileList;
                                 });
var webkit_dom_file_list_item;
webkit_dom_file_list_item = (function(self, index)
                             {
                               return self["item"]($hs_intToNumber(index));
                             });
var webkit_dom_file_list_get_length;
webkit_dom_file_list_get_length = (function(self)
                                   {
                                     return $hs_int(self["length"]);
                                   });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_file_get_type = (function()
                            {
                              return File;
                            });
var webkit_dom_file_get_name;
webkit_dom_file_get_name = (function(self)
                            {
                              return $hs_toUtf8(self["name"]);
                            });
var webkit_dom_file_get_file_name;
webkit_dom_file_get_file_name = (function(self)
                                 {
                                   return $hs_toUtf8(self["fileName"]);
                                 });
var webkit_dom_file_get_file_size;
webkit_dom_file_get_file_size = (function(self)
                                 {
                                   return $hs_int(self["fileSize"]);
                                 });
// Graphics.UI.Gtk.WebKit.DOM.Events
webkit_dom_event_target_get_type = (function()
                                    {
                                      return EventTarget;
                                    });
var webkit_dom_event_target_dispatch_event;
webkit_dom_event_target_dispatch_event = (function(self, event)
                                          {
                                            return $hs_int((self["dispatchEvent"](event) ? 1 : 0));
                                          });
// Graphics.UI.Gtk.WebKit.DOM.Events
webkit_dom_event_get_type = (function()
                             {
                               return Event;
                             });
var webkit_dom_event_stop_propagation;
webkit_dom_event_stop_propagation = (function(self)
                                     {
                                       return self["stopPropagation"]();
                                     });
var webkit_dom_event_prevent_default;
webkit_dom_event_prevent_default = (function(self)
                                    {
                                      return self["preventDefault"]();
                                    });
var webkit_dom_event_init_event;
webkit_dom_event_init_event = (function(self, eventTypeArg,
                                        canBubbleArg, cancelableArg)
                               {
                                 return self["initEvent"]($hs_fromUtf8(eventTypeArg),
                                                          ($hs_intToNumber(canBubbleArg) != 0),
                                                          ($hs_intToNumber(cancelableArg) != 0));
                               });
var webkit_dom_event_stop_immediate_propagation;
webkit_dom_event_stop_immediate_propagation = (function(self)
                                               {
                                                 return self["stopImmediatePropagation"]();
                                               });
var webkit_dom_event_get_target;
webkit_dom_event_get_target = (function(self)
                               {
                                 return self["target"];
                               });
var webkit_dom_event_get_current_target;
webkit_dom_event_get_current_target = (function(self)
                                       {
                                         return self["currentTarget"];
                                       });
var webkit_dom_event_get_event_phase;
webkit_dom_event_get_event_phase = (function(self)
                                    {
                                      return $hs_int(self["eventPhase"]);
                                    });
var webkit_dom_event_get_bubbles;
webkit_dom_event_get_bubbles = (function(self)
                                {
                                  return $hs_int((self["bubbles"] ? 1 : 0));
                                });
var webkit_dom_event_get_cancelable;
webkit_dom_event_get_cancelable = (function(self)
                                   {
                                     return $hs_int((self["cancelable"] ? 1 : 0));
                                   });
var webkit_dom_event_get_time_stamp;
webkit_dom_event_get_time_stamp = (function(self)
                                   {
                                     return $hs_int(self["timeStamp"]);
                                   });
var webkit_dom_event_get_default_prevented;
webkit_dom_event_get_default_prevented = (function(self)
                                          {
                                            return $hs_int((self["defaultPrevented"] ? 1 : 0));
                                          });
var webkit_dom_event_get_src_element;
webkit_dom_event_get_src_element = (function(self)
                                    {
                                      return self["srcElement"];
                                    });
var webkit_dom_event_set_return_value;
webkit_dom_event_set_return_value = (function(self, val)
                                     {
                                       self["returnValue"] = ($hs_intToNumber(val) != 0);
                                     });
var webkit_dom_event_get_return_value;
webkit_dom_event_get_return_value = (function(self)
                                     {
                                       return $hs_int((self["returnValue"] ? 1 : 0));
                                     });
var webkit_dom_event_set_cancel_bubble;
webkit_dom_event_set_cancel_bubble = (function(self, val)
                                      {
                                        self["cancelBubble"] = ($hs_intToNumber(val) != 0);
                                      });
var webkit_dom_event_get_cancel_bubble;
webkit_dom_event_get_cancel_bubble = (function(self)
                                      {
                                        return $hs_int((self["cancelBubble"] ? 1 : 0));
                                      });
// Graphics.UI.Gtk.WebKit.DOM.Core
webkit_dom_entity_reference_get_type = (function()
                                        {
                                          return EntityReference;
                                        });
// Graphics.UI.Gtk.WebKit.DOM.Core
webkit_dom_element_get_type = (function()
                               {
                                 return Element;
                               });
var webkit_dom_element_get_attribute;
webkit_dom_element_get_attribute = (function(self, name)
                                    {
                                      return $hs_toUtf8(self["getAttribute"]($hs_fromUtf8(name)));
                                    });
var webkit_dom_element_set_attribute;
webkit_dom_element_set_attribute = (function(self, name, value)
                                    {
                                      return self["setAttribute"]($hs_fromUtf8(name),
                                                                  $hs_fromUtf8(value));
                                    });
var webkit_dom_element_remove_attribute;
webkit_dom_element_remove_attribute = (function(self, name)
                                       {
                                         return self["removeAttribute"]($hs_fromUtf8(name));
                                       });
var webkit_dom_element_get_attribute_node;
webkit_dom_element_get_attribute_node = (function(self, name)
                                         {
                                           return self["getAttributeNode"]($hs_fromUtf8(name));
                                         });
var webkit_dom_element_set_attribute_node;
webkit_dom_element_set_attribute_node = (function(self, newAttr)
                                         {
                                           return self["setAttributeNode"](newAttr);
                                         });
var webkit_dom_element_remove_attribute_node;
webkit_dom_element_remove_attribute_node = (function(self, oldAttr)
                                            {
                                              return self["removeAttributeNode"](oldAttr);
                                            });
var webkit_dom_element_get_elements_by_tag_name;
webkit_dom_element_get_elements_by_tag_name = (function(self, name)
                                               {
                                                 return self["getElementsByTagName"]($hs_fromUtf8(name));
                                               });
var webkit_dom_element_get_attribute_ns;
webkit_dom_element_get_attribute_ns = (function(self, namespaceURI,
                                                localName)
                                       {
                                         return $hs_toUtf8(self["getAttributeNS"]($hs_fromUtf8(namespaceURI),
                                                                                  $hs_fromUtf8(localName)));
                                       });
var webkit_dom_element_set_attribute_ns;
webkit_dom_element_set_attribute_ns = (function(self, namespaceURI,
                                                qualifiedName, value)
                                       {
                                         return self["setAttributeNS"]($hs_fromUtf8(namespaceURI),
                                                                       $hs_fromUtf8(qualifiedName),
                                                                       $hs_fromUtf8(value));
                                       });
var webkit_dom_element_remove_attribute_ns;
webkit_dom_element_remove_attribute_ns = (function(self,
                                                   namespaceURI, localName)
                                          {
                                            return self["removeAttributeNS"]($hs_fromUtf8(namespaceURI),
                                                                             $hs_fromUtf8(localName));
                                          });
var webkit_dom_element_get_elements_by_tag_name_ns;
webkit_dom_element_get_elements_by_tag_name_ns = (function(self,
                                                           namespaceURI, localName)
                                                  {
                                                    return self["getElementsByTagNameNS"]($hs_fromUtf8(namespaceURI),
                                                                                          $hs_fromUtf8(localName));
                                                  });
var webkit_dom_element_get_attribute_node_ns;
webkit_dom_element_get_attribute_node_ns = (function(self,
                                                     namespaceURI, localName)
                                            {
                                              return self["getAttributeNodeNS"]($hs_fromUtf8(namespaceURI),
                                                                                $hs_fromUtf8(localName));
                                            });
var webkit_dom_element_set_attribute_node_ns;
webkit_dom_element_set_attribute_node_ns = (function(self, newAttr)
                                            {
                                              return self["setAttributeNodeNS"](newAttr);
                                            });
var webkit_dom_element_has_attribute;
webkit_dom_element_has_attribute = (function(self, name)
                                    {
                                      return $hs_int((self["hasAttribute"]($hs_fromUtf8(name)) ? 1 : 0));
                                    });
var webkit_dom_element_has_attribute_ns;
webkit_dom_element_has_attribute_ns = (function(self, namespaceURI,
                                                localName)
                                       {
                                         return $hs_int((self["hasAttributeNS"]($hs_fromUtf8(namespaceURI),
                                                                                $hs_fromUtf8(localName)) ? 1 : 0));
                                       });
var webkit_dom_element_focus;
webkit_dom_element_focus = (function(self)
                            {
                              return self["focus"]();
                            });
var webkit_dom_element_blur;
webkit_dom_element_blur = (function(self)
                           {
                             return self["blur"]();
                           });
var webkit_dom_element_scroll_into_view;
webkit_dom_element_scroll_into_view = (function(self, alignWithTop)
                                       {
                                         return self["scrollIntoView"](($hs_intToNumber(alignWithTop)
                                                                        !=
                                                                        0));
                                       });
var webkit_dom_element_scroll_into_view_if_needed;
webkit_dom_element_scroll_into_view_if_needed = (function(self,
                                                          centerIfNeeded)
                                                 {
                                                   return self["scrollIntoViewIfNeeded"](($hs_intToNumber(centerIfNeeded)
                                                                                          !=
                                                                                          0));
                                                 });
var webkit_dom_element_scroll_by_lines;
webkit_dom_element_scroll_by_lines = (function(self, lines)
                                      {
                                        return self["scrollByLines"]($hs_intToNumber(lines));
                                      });
var webkit_dom_element_scroll_by_pages;
webkit_dom_element_scroll_by_pages = (function(self, pages)
                                      {
                                        return self["scrollByPages"]($hs_intToNumber(pages));
                                      });
var webkit_dom_element_get_elements_by_class_name;
webkit_dom_element_get_elements_by_class_name = (function(self,
                                                          name)
                                                 {
                                                   return self["getElementsByClassName"]($hs_fromUtf8(name));
                                                 });
var webkit_dom_element_query_selector;
webkit_dom_element_query_selector = (function(self, selectors)
                                     {
                                       return self["querySelector"]($hs_fromUtf8(selectors));
                                     });
var webkit_dom_element_query_selector_all;
webkit_dom_element_query_selector_all = (function(self, selectors)
                                         {
                                           return self["querySelectorAll"]($hs_fromUtf8(selectors));
                                         });
var webkit_dom_element_webkit_matches_selector;
webkit_dom_element_webkit_matches_selector = (function(self,
                                                       selectors)
                                              {
                                                return $hs_int((self["webkitMatchesSelector"]($hs_fromUtf8(selectors)) ? 1 : 0));
                                              });
var webkit_dom_element_get_tag_name;
webkit_dom_element_get_tag_name = (function(self)
                                   {
                                     return $hs_toUtf8(self["tagName"]);
                                   });
var webkit_dom_element_get_style;
webkit_dom_element_get_style = (function(self)
                                {
                                  return self["style"];
                                });
var webkit_dom_element_get_offset_left;
webkit_dom_element_get_offset_left = (function(self)
                                      {
                                        return $hs_int(self["offsetLeft"]);
                                      });
var webkit_dom_element_get_offset_top;
webkit_dom_element_get_offset_top = (function(self)
                                     {
                                       return $hs_int(self["offsetTop"]);
                                     });
var webkit_dom_element_get_offset_width;
webkit_dom_element_get_offset_width = (function(self)
                                       {
                                         return $hs_int(self["offsetWidth"]);
                                       });
var webkit_dom_element_get_offset_height;
webkit_dom_element_get_offset_height = (function(self)
                                        {
                                          return $hs_int(self["offsetHeight"]);
                                        });
var webkit_dom_element_get_offset_parent;
webkit_dom_element_get_offset_parent = (function(self)
                                        {
                                          return self["offsetParent"];
                                        });
var webkit_dom_element_get_client_left;
webkit_dom_element_get_client_left = (function(self)
                                      {
                                        return $hs_int(self["clientLeft"]);
                                      });
var webkit_dom_element_get_client_top;
webkit_dom_element_get_client_top = (function(self)
                                     {
                                       return $hs_int(self["clientTop"]);
                                     });
var webkit_dom_element_get_client_width;
webkit_dom_element_get_client_width = (function(self)
                                       {
                                         return $hs_int(self["clientWidth"]);
                                       });
var webkit_dom_element_get_client_height;
webkit_dom_element_get_client_height = (function(self)
                                        {
                                          return $hs_int(self["clientHeight"]);
                                        });
var webkit_dom_element_set_scroll_left;
webkit_dom_element_set_scroll_left = (function(self, val)
                                      {
                                        self["scrollLeft"] = $hs_intToNumber(val);
                                      });
var webkit_dom_element_get_scroll_left;
webkit_dom_element_get_scroll_left = (function(self)
                                      {
                                        return $hs_int(self["scrollLeft"]);
                                      });
var webkit_dom_element_set_scroll_top;
webkit_dom_element_set_scroll_top = (function(self, val)
                                     {
                                       self["scrollTop"] = $hs_intToNumber(val);
                                     });
var webkit_dom_element_get_scroll_top;
webkit_dom_element_get_scroll_top = (function(self)
                                     {
                                       return $hs_int(self["scrollTop"]);
                                     });
var webkit_dom_element_get_scroll_width;
webkit_dom_element_get_scroll_width = (function(self)
                                       {
                                         return $hs_int(self["scrollWidth"]);
                                       });
var webkit_dom_element_get_scroll_height;
webkit_dom_element_get_scroll_height = (function(self)
                                        {
                                          return $hs_int(self["scrollHeight"]);
                                        });
var webkit_dom_element_get_first_element_child;
webkit_dom_element_get_first_element_child = (function(self)
                                              {
                                                return self["firstElementChild"];
                                              });
var webkit_dom_element_get_last_element_child;
webkit_dom_element_get_last_element_child = (function(self)
                                             {
                                               return self["lastElementChild"];
                                             });
var webkit_dom_element_get_previous_element_sibling;
webkit_dom_element_get_previous_element_sibling = (function(self)
                                                   {
                                                     return self["previousElementSibling"];
                                                   });
var webkit_dom_element_get_next_element_sibling;
webkit_dom_element_get_next_element_sibling = (function(self)
                                               {
                                                 return self["nextElementSibling"];
                                               });
var webkit_dom_element_get_child_element_count;
webkit_dom_element_get_child_element_count = (function(self)
                                              {
                                                return $hs_int(self["childElementCount"]);
                                              });
var webkit_dom_element_get_webkit_region_overflow;
webkit_dom_element_get_webkit_region_overflow = (function(self)
                                                 {
                                                   return $hs_toUtf8(self["webkitRegionOverflow"]);
                                                 });
var webkit_dom_element_set_onabort;
webkit_dom_element_set_onabort = (function(self, val)
                                  {
                                    self["onabort"] = val;
                                  });
var webkit_dom_element_get_onabort;
webkit_dom_element_get_onabort = (function(self)
                                  {
                                    return self["onabort"];
                                  });
var webkit_dom_element_set_onblur;
webkit_dom_element_set_onblur = (function(self, val)
                                 {
                                   self["onblur"] = val;
                                 });
var webkit_dom_element_get_onblur;
webkit_dom_element_get_onblur = (function(self)
                                 {
                                   return self["onblur"];
                                 });
var webkit_dom_element_set_onchange;
webkit_dom_element_set_onchange = (function(self, val)
                                   {
                                     self["onchange"] = val;
                                   });
var webkit_dom_element_get_onchange;
webkit_dom_element_get_onchange = (function(self)
                                   {
                                     return self["onchange"];
                                   });
var webkit_dom_element_set_onclick;
webkit_dom_element_set_onclick = (function(self, val)
                                  {
                                    self["onclick"] = val;
                                  });
var webkit_dom_element_get_onclick;
webkit_dom_element_get_onclick = (function(self)
                                  {
                                    return self["onclick"];
                                  });
var webkit_dom_element_set_oncontextmenu;
webkit_dom_element_set_oncontextmenu = (function(self, val)
                                        {
                                          self["oncontextmenu"] = val;
                                        });
var webkit_dom_element_get_oncontextmenu;
webkit_dom_element_get_oncontextmenu = (function(self)
                                        {
                                          return self["oncontextmenu"];
                                        });
var webkit_dom_element_set_ondblclick;
webkit_dom_element_set_ondblclick = (function(self, val)
                                     {
                                       self["ondblclick"] = val;
                                     });
var webkit_dom_element_get_ondblclick;
webkit_dom_element_get_ondblclick = (function(self)
                                     {
                                       return self["ondblclick"];
                                     });
var webkit_dom_element_set_ondrag;
webkit_dom_element_set_ondrag = (function(self, val)
                                 {
                                   self["ondrag"] = val;
                                 });
var webkit_dom_element_get_ondrag;
webkit_dom_element_get_ondrag = (function(self)
                                 {
                                   return self["ondrag"];
                                 });
var webkit_dom_element_set_ondragend;
webkit_dom_element_set_ondragend = (function(self, val)
                                    {
                                      self["ondragend"] = val;
                                    });
var webkit_dom_element_get_ondragend;
webkit_dom_element_get_ondragend = (function(self)
                                    {
                                      return self["ondragend"];
                                    });
var webkit_dom_element_set_ondragenter;
webkit_dom_element_set_ondragenter = (function(self, val)
                                      {
                                        self["ondragenter"] = val;
                                      });
var webkit_dom_element_get_ondragenter;
webkit_dom_element_get_ondragenter = (function(self)
                                      {
                                        return self["ondragenter"];
                                      });
var webkit_dom_element_set_ondragleave;
webkit_dom_element_set_ondragleave = (function(self, val)
                                      {
                                        self["ondragleave"] = val;
                                      });
var webkit_dom_element_get_ondragleave;
webkit_dom_element_get_ondragleave = (function(self)
                                      {
                                        return self["ondragleave"];
                                      });
var webkit_dom_element_set_ondragover;
webkit_dom_element_set_ondragover = (function(self, val)
                                     {
                                       self["ondragover"] = val;
                                     });
var webkit_dom_element_get_ondragover;
webkit_dom_element_get_ondragover = (function(self)
                                     {
                                       return self["ondragover"];
                                     });
var webkit_dom_element_set_ondragstart;
webkit_dom_element_set_ondragstart = (function(self, val)
                                      {
                                        self["ondragstart"] = val;
                                      });
var webkit_dom_element_get_ondragstart;
webkit_dom_element_get_ondragstart = (function(self)
                                      {
                                        return self["ondragstart"];
                                      });
var webkit_dom_element_set_ondrop;
webkit_dom_element_set_ondrop = (function(self, val)
                                 {
                                   self["ondrop"] = val;
                                 });
var webkit_dom_element_get_ondrop;
webkit_dom_element_get_ondrop = (function(self)
                                 {
                                   return self["ondrop"];
                                 });
var webkit_dom_element_set_onerror;
webkit_dom_element_set_onerror = (function(self, val)
                                  {
                                    self["onerror"] = val;
                                  });
var webkit_dom_element_get_onerror;
webkit_dom_element_get_onerror = (function(self)
                                  {
                                    return self["onerror"];
                                  });
var webkit_dom_element_set_onfocus;
webkit_dom_element_set_onfocus = (function(self, val)
                                  {
                                    self["onfocus"] = val;
                                  });
var webkit_dom_element_get_onfocus;
webkit_dom_element_get_onfocus = (function(self)
                                  {
                                    return self["onfocus"];
                                  });
var webkit_dom_element_set_oninput;
webkit_dom_element_set_oninput = (function(self, val)
                                  {
                                    self["oninput"] = val;
                                  });
var webkit_dom_element_get_oninput;
webkit_dom_element_get_oninput = (function(self)
                                  {
                                    return self["oninput"];
                                  });
var webkit_dom_element_set_oninvalid;
webkit_dom_element_set_oninvalid = (function(self, val)
                                    {
                                      self["oninvalid"] = val;
                                    });
var webkit_dom_element_get_oninvalid;
webkit_dom_element_get_oninvalid = (function(self)
                                    {
                                      return self["oninvalid"];
                                    });
var webkit_dom_element_set_onkeydown;
webkit_dom_element_set_onkeydown = (function(self, val)
                                    {
                                      self["onkeydown"] = val;
                                    });
var webkit_dom_element_get_onkeydown;
webkit_dom_element_get_onkeydown = (function(self)
                                    {
                                      return self["onkeydown"];
                                    });
var webkit_dom_element_set_onkeypress;
webkit_dom_element_set_onkeypress = (function(self, val)
                                     {
                                       self["onkeypress"] = val;
                                     });
var webkit_dom_element_get_onkeypress;
webkit_dom_element_get_onkeypress = (function(self)
                                     {
                                       return self["onkeypress"];
                                     });
var webkit_dom_element_set_onkeyup;
webkit_dom_element_set_onkeyup = (function(self, val)
                                  {
                                    self["onkeyup"] = val;
                                  });
var webkit_dom_element_get_onkeyup;
webkit_dom_element_get_onkeyup = (function(self)
                                  {
                                    return self["onkeyup"];
                                  });
var webkit_dom_element_set_onload;
webkit_dom_element_set_onload = (function(self, val)
                                 {
                                   self["onload"] = val;
                                 });
var webkit_dom_element_get_onload;
webkit_dom_element_get_onload = (function(self)
                                 {
                                   return self["onload"];
                                 });
var webkit_dom_element_set_onmousedown;
webkit_dom_element_set_onmousedown = (function(self, val)
                                      {
                                        self["onmousedown"] = val;
                                      });
var webkit_dom_element_get_onmousedown;
webkit_dom_element_get_onmousedown = (function(self)
                                      {
                                        return self["onmousedown"];
                                      });
var webkit_dom_element_set_onmousemove;
webkit_dom_element_set_onmousemove = (function(self, val)
                                      {
                                        self["onmousemove"] = val;
                                      });
var webkit_dom_element_get_onmousemove;
webkit_dom_element_get_onmousemove = (function(self)
                                      {
                                        return self["onmousemove"];
                                      });
var webkit_dom_element_set_onmouseout;
webkit_dom_element_set_onmouseout = (function(self, val)
                                     {
                                       self["onmouseout"] = val;
                                     });
var webkit_dom_element_get_onmouseout;
webkit_dom_element_get_onmouseout = (function(self)
                                     {
                                       return self["onmouseout"];
                                     });
var webkit_dom_element_set_onmouseover;
webkit_dom_element_set_onmouseover = (function(self, val)
                                      {
                                        self["onmouseover"] = val;
                                      });
var webkit_dom_element_get_onmouseover;
webkit_dom_element_get_onmouseover = (function(self)
                                      {
                                        return self["onmouseover"];
                                      });
var webkit_dom_element_set_onmouseup;
webkit_dom_element_set_onmouseup = (function(self, val)
                                    {
                                      self["onmouseup"] = val;
                                    });
var webkit_dom_element_get_onmouseup;
webkit_dom_element_get_onmouseup = (function(self)
                                    {
                                      return self["onmouseup"];
                                    });
var webkit_dom_element_set_onmousewheel;
webkit_dom_element_set_onmousewheel = (function(self, val)
                                       {
                                         self["onmousewheel"] = val;
                                       });
var webkit_dom_element_get_onmousewheel;
webkit_dom_element_get_onmousewheel = (function(self)
                                       {
                                         return self["onmousewheel"];
                                       });
var webkit_dom_element_set_onscroll;
webkit_dom_element_set_onscroll = (function(self, val)
                                   {
                                     self["onscroll"] = val;
                                   });
var webkit_dom_element_get_onscroll;
webkit_dom_element_get_onscroll = (function(self)
                                   {
                                     return self["onscroll"];
                                   });
var webkit_dom_element_set_onselect;
webkit_dom_element_set_onselect = (function(self, val)
                                   {
                                     self["onselect"] = val;
                                   });
var webkit_dom_element_get_onselect;
webkit_dom_element_get_onselect = (function(self)
                                   {
                                     return self["onselect"];
                                   });
var webkit_dom_element_set_onsubmit;
webkit_dom_element_set_onsubmit = (function(self, val)
                                   {
                                     self["onsubmit"] = val;
                                   });
var webkit_dom_element_get_onsubmit;
webkit_dom_element_get_onsubmit = (function(self)
                                   {
                                     return self["onsubmit"];
                                   });
var webkit_dom_element_set_onbeforecut;
webkit_dom_element_set_onbeforecut = (function(self, val)
                                      {
                                        self["onbeforecut"] = val;
                                      });
var webkit_dom_element_get_onbeforecut;
webkit_dom_element_get_onbeforecut = (function(self)
                                      {
                                        return self["onbeforecut"];
                                      });
var webkit_dom_element_set_oncut;
webkit_dom_element_set_oncut = (function(self, val)
                                {
                                  self["oncut"] = val;
                                });
var webkit_dom_element_get_oncut;
webkit_dom_element_get_oncut = (function(self)
                                {
                                  return self["oncut"];
                                });
var webkit_dom_element_set_onbeforecopy;
webkit_dom_element_set_onbeforecopy = (function(self, val)
                                       {
                                         self["onbeforecopy"] = val;
                                       });
var webkit_dom_element_get_onbeforecopy;
webkit_dom_element_get_onbeforecopy = (function(self)
                                       {
                                         return self["onbeforecopy"];
                                       });
var webkit_dom_element_set_oncopy;
webkit_dom_element_set_oncopy = (function(self, val)
                                 {
                                   self["oncopy"] = val;
                                 });
var webkit_dom_element_get_oncopy;
webkit_dom_element_get_oncopy = (function(self)
                                 {
                                   return self["oncopy"];
                                 });
var webkit_dom_element_set_onbeforepaste;
webkit_dom_element_set_onbeforepaste = (function(self, val)
                                        {
                                          self["onbeforepaste"] = val;
                                        });
var webkit_dom_element_get_onbeforepaste;
webkit_dom_element_get_onbeforepaste = (function(self)
                                        {
                                          return self["onbeforepaste"];
                                        });
var webkit_dom_element_set_onpaste;
webkit_dom_element_set_onpaste = (function(self, val)
                                  {
                                    self["onpaste"] = val;
                                  });
var webkit_dom_element_get_onpaste;
webkit_dom_element_get_onpaste = (function(self)
                                  {
                                    return self["onpaste"];
                                  });
var webkit_dom_element_set_onreset;
webkit_dom_element_set_onreset = (function(self, val)
                                  {
                                    self["onreset"] = val;
                                  });
var webkit_dom_element_get_onreset;
webkit_dom_element_get_onreset = (function(self)
                                  {
                                    return self["onreset"];
                                  });
var webkit_dom_element_set_onsearch;
webkit_dom_element_set_onsearch = (function(self, val)
                                   {
                                     self["onsearch"] = val;
                                   });
var webkit_dom_element_get_onsearch;
webkit_dom_element_get_onsearch = (function(self)
                                   {
                                     return self["onsearch"];
                                   });
var webkit_dom_element_set_onselectstart;
webkit_dom_element_set_onselectstart = (function(self, val)
                                        {
                                          self["onselectstart"] = val;
                                        });
var webkit_dom_element_get_onselectstart;
webkit_dom_element_get_onselectstart = (function(self)
                                        {
                                          return self["onselectstart"];
                                        });
var webkit_dom_element_set_ontouchstart;
webkit_dom_element_set_ontouchstart = (function(self, val)
                                       {
                                         self["ontouchstart"] = val;
                                       });
var webkit_dom_element_get_ontouchstart;
webkit_dom_element_get_ontouchstart = (function(self)
                                       {
                                         return self["ontouchstart"];
                                       });
var webkit_dom_element_set_ontouchmove;
webkit_dom_element_set_ontouchmove = (function(self, val)
                                      {
                                        self["ontouchmove"] = val;
                                      });
var webkit_dom_element_get_ontouchmove;
webkit_dom_element_get_ontouchmove = (function(self)
                                      {
                                        return self["ontouchmove"];
                                      });
var webkit_dom_element_set_ontouchend;
webkit_dom_element_set_ontouchend = (function(self, val)
                                     {
                                       self["ontouchend"] = val;
                                     });
var webkit_dom_element_get_ontouchend;
webkit_dom_element_get_ontouchend = (function(self)
                                     {
                                       return self["ontouchend"];
                                     });
var webkit_dom_element_set_ontouchcancel;
webkit_dom_element_set_ontouchcancel = (function(self, val)
                                        {
                                          self["ontouchcancel"] = val;
                                        });
var webkit_dom_element_get_ontouchcancel;
webkit_dom_element_get_ontouchcancel = (function(self)
                                        {
                                          return self["ontouchcancel"];
                                        });
var webkit_dom_element_set_onwebkitfullscreenchange;
webkit_dom_element_set_onwebkitfullscreenchange = (function(self,
                                                            val)
                                                   {
                                                     self["onwebkitfullscreenchange"] = val;
                                                   });
var webkit_dom_element_get_onwebkitfullscreenchange;
webkit_dom_element_get_onwebkitfullscreenchange = (function(self)
                                                   {
                                                     return self["onwebkitfullscreenchange"];
                                                   });
var webkit_dom_element_set_onwebkitfullscreenerror;
webkit_dom_element_set_onwebkitfullscreenerror = (function(self,
                                                           val)
                                                  {
                                                    self["onwebkitfullscreenerror"] = val;
                                                  });
var webkit_dom_element_get_onwebkitfullscreenerror;
webkit_dom_element_get_onwebkitfullscreenerror = (function(self)
                                                  {
                                                    return self["onwebkitfullscreenerror"];
                                                  });
// Graphics.UI.Gtk.WebKit.DOM.Window
webkit_dom_dom_window_get_type = (function()
                                  {
                                    return Window;
                                  });
var webkit_dom_dom_window_get_selection;
webkit_dom_dom_window_get_selection = (function(self)
                                       {
                                         return self["getSelection"]();
                                       });
var webkit_dom_dom_window_focus;
webkit_dom_dom_window_focus = (function(self)
                               {
                                 return self["focus"]();
                               });
var webkit_dom_dom_window_blur;
webkit_dom_dom_window_blur = (function(self)
                              {
                                return self["blur"]();
                              });
var webkit_dom_dom_window_print;
webkit_dom_dom_window_print = (function(self)
                               {
                                 return self["print"]();
                               });
var webkit_dom_dom_window_stop;
webkit_dom_dom_window_stop = (function(self)
                              {
                                return self["stop"]();
                              });
var webkit_dom_dom_window_alert;
webkit_dom_dom_window_alert = (function(self, message)
                               {
                                 return self["alert"]($hs_fromUtf8(message));
                               });
var webkit_dom_dom_window_confirm;
webkit_dom_dom_window_confirm = (function(self, message)
                                 {
                                   return $hs_int((self["confirm"]($hs_fromUtf8(message)) ? 1 : 0));
                                 });
var webkit_dom_dom_window_prompt;
webkit_dom_dom_window_prompt = (function(self, message,
                                         defaultValue)
                                {
                                  return $hs_toUtf8(self["prompt"]($hs_fromUtf8(message),
                                                                   $hs_fromUtf8(defaultValue)));
                                });
var webkit_dom_dom_window_find;
webkit_dom_dom_window_find = (function(self, string, caseSensitive,
                                       backwards, wrap, wholeWord, searchInFrames, showDialog)
                              {
                                return $hs_int((self["find"]($hs_fromUtf8(string),
                                                             ($hs_intToNumber(caseSensitive) != 0),
                                                             ($hs_intToNumber(backwards) != 0),
                                                             ($hs_intToNumber(wrap) != 0),
                                                             ($hs_intToNumber(wholeWord) != 0),
                                                             ($hs_intToNumber(searchInFrames) != 0),
                                                             ($hs_intToNumber(showDialog)
                                                              !=
                                                              0)) ? 1 : 0));
                              });
var webkit_dom_dom_window_scroll_by;
webkit_dom_dom_window_scroll_by = (function(self, x, y)
                                   {
                                     return self["scrollBy"]($hs_intToNumber(x),
                                                             $hs_intToNumber(y));
                                   });
var webkit_dom_dom_window_scroll_to;
webkit_dom_dom_window_scroll_to = (function(self, x, y)
                                   {
                                     return self["scrollTo"]($hs_intToNumber(x),
                                                             $hs_intToNumber(y));
                                   });
var webkit_dom_dom_window_scroll;
webkit_dom_dom_window_scroll = (function(self, x, y)
                                {
                                  return self["scroll"]($hs_intToNumber(x), $hs_intToNumber(y));
                                });
var webkit_dom_dom_window_move_by;
webkit_dom_dom_window_move_by = (function(self, x, y)
                                 {
                                   return self["moveBy"](x, y);
                                 });
var webkit_dom_dom_window_move_to;
webkit_dom_dom_window_move_to = (function(self, x, y)
                                 {
                                   return self["moveTo"](x, y);
                                 });
var webkit_dom_dom_window_resize_by;
webkit_dom_dom_window_resize_by = (function(self, x, y)
                                   {
                                     return self["resizeBy"](x, y);
                                   });
var webkit_dom_dom_window_resize_to;
webkit_dom_dom_window_resize_to = (function(self, width, height)
                                   {
                                     return self["resizeTo"](width, height);
                                   });
var webkit_dom_dom_window_match_media;
webkit_dom_dom_window_match_media = (function(self, query)
                                     {
                                       return self["matchMedia"]($hs_fromUtf8(query));
                                     });
var webkit_dom_dom_window_get_computed_style;
webkit_dom_dom_window_get_computed_style = (function(self, element,
                                                     pseudoElement)
                                            {
                                              return self["getComputedStyle"](element,
                                                                              $hs_fromUtf8(pseudoElement));
                                            });
var webkit_dom_dom_window_webkit_convert_point_from_page_to_node;
webkit_dom_dom_window_webkit_convert_point_from_page_to_node = (function(self,
                                                                         node, p)
                                                                {
                                                                  return self["webkitConvertPointFromPageToNode"](node,
                                                                                                                  p);
                                                                });
var webkit_dom_dom_window_webkit_convert_point_from_node_to_page;
webkit_dom_dom_window_webkit_convert_point_from_node_to_page = (function(self,
                                                                         node, p)
                                                                {
                                                                  return self["webkitConvertPointFromNodeToPage"](node,
                                                                                                                  p);
                                                                });
var webkit_dom_dom_window_clear_timeout;
webkit_dom_dom_window_clear_timeout = (function(self, handle)
                                       {
                                         return self["clearTimeout"]($hs_intToNumber(handle));
                                       });
var webkit_dom_dom_window_clear_interval;
webkit_dom_dom_window_clear_interval = (function(self, handle)
                                        {
                                          return self["clearInterval"]($hs_intToNumber(handle));
                                        });
var webkit_dom_dom_window_atob;
webkit_dom_dom_window_atob = (function(self, string)
                              {
                                return $hs_toUtf8(self["atob"]($hs_fromUtf8(string)));
                              });
var webkit_dom_dom_window_btoa;
webkit_dom_dom_window_btoa = (function(self, string)
                              {
                                return $hs_toUtf8(self["btoa"]($hs_fromUtf8(string)));
                              });
var webkit_dom_dom_window_dispatch_event;
webkit_dom_dom_window_dispatch_event = (function(self, evt)
                                        {
                                          return $hs_int((self["dispatchEvent"](evt) ? 1 : 0));
                                        });
var webkit_dom_dom_window_capture_events;
webkit_dom_dom_window_capture_events = (function(self)
                                        {
                                          return self["captureEvents"]();
                                        });
var webkit_dom_dom_window_release_events;
webkit_dom_dom_window_release_events = (function(self)
                                        {
                                          return self["releaseEvents"]();
                                        });
var webkit_dom_dom_window_get_screen;
webkit_dom_dom_window_get_screen = (function(self)
                                    {
                                      return self["screen"];
                                    });
var webkit_dom_dom_window_get_history;
webkit_dom_dom_window_get_history = (function(self)
                                     {
                                       return self["history"];
                                     });
var webkit_dom_dom_window_get_locationbar;
webkit_dom_dom_window_get_locationbar = (function(self)
                                         {
                                           return self["locationbar"];
                                         });
var webkit_dom_dom_window_get_menubar;
webkit_dom_dom_window_get_menubar = (function(self)
                                     {
                                       return self["menubar"];
                                     });
var webkit_dom_dom_window_get_personalbar;
webkit_dom_dom_window_get_personalbar = (function(self)
                                         {
                                           return self["personalbar"];
                                         });
var webkit_dom_dom_window_get_scrollbars;
webkit_dom_dom_window_get_scrollbars = (function(self)
                                        {
                                          return self["scrollbars"];
                                        });
var webkit_dom_dom_window_get_statusbar;
webkit_dom_dom_window_get_statusbar = (function(self)
                                       {
                                         return self["statusbar"];
                                       });
var webkit_dom_dom_window_get_toolbar;
webkit_dom_dom_window_get_toolbar = (function(self)
                                     {
                                       return self["toolbar"];
                                     });
var webkit_dom_dom_window_get_navigator;
webkit_dom_dom_window_get_navigator = (function(self)
                                       {
                                         return self["navigator"];
                                       });
var webkit_dom_dom_window_get_client_information;
webkit_dom_dom_window_get_client_information = (function(self)
                                                {
                                                  return self["clientInformation"];
                                                });
var webkit_dom_dom_window_get_frame_element;
webkit_dom_dom_window_get_frame_element = (function(self)
                                           {
                                             return self["frameElement"];
                                           });
var webkit_dom_dom_window_get_offscreen_buffering;
webkit_dom_dom_window_get_offscreen_buffering = (function(self)
                                                 {
                                                   return $hs_int((self["offscreenBuffering"] ? 1 : 0));
                                                 });
var webkit_dom_dom_window_get_outer_height;
webkit_dom_dom_window_get_outer_height = (function(self)
                                          {
                                            return $hs_int(self["outerHeight"]);
                                          });
var webkit_dom_dom_window_get_outer_width;
webkit_dom_dom_window_get_outer_width = (function(self)
                                         {
                                           return $hs_int(self["outerWidth"]);
                                         });
var webkit_dom_dom_window_get_inner_height;
webkit_dom_dom_window_get_inner_height = (function(self)
                                          {
                                            return $hs_int(self["innerHeight"]);
                                          });
var webkit_dom_dom_window_get_inner_width;
webkit_dom_dom_window_get_inner_width = (function(self)
                                         {
                                           return $hs_int(self["innerWidth"]);
                                         });
var webkit_dom_dom_window_get_screen_x;
webkit_dom_dom_window_get_screen_x = (function(self)
                                      {
                                        return $hs_int(self["screenX"]);
                                      });
var webkit_dom_dom_window_get_screen_y;
webkit_dom_dom_window_get_screen_y = (function(self)
                                      {
                                        return $hs_int(self["screenY"]);
                                      });
var webkit_dom_dom_window_get_screen_left;
webkit_dom_dom_window_get_screen_left = (function(self)
                                         {
                                           return $hs_int(self["screenLeft"]);
                                         });
var webkit_dom_dom_window_get_screen_top;
webkit_dom_dom_window_get_screen_top = (function(self)
                                        {
                                          return $hs_int(self["screenTop"]);
                                        });
var webkit_dom_dom_window_get_scroll_x;
webkit_dom_dom_window_get_scroll_x = (function(self)
                                      {
                                        return $hs_int(self["scrollX"]);
                                      });
var webkit_dom_dom_window_get_scroll_y;
webkit_dom_dom_window_get_scroll_y = (function(self)
                                      {
                                        return $hs_int(self["scrollY"]);
                                      });
var webkit_dom_dom_window_get_page_x_offset;
webkit_dom_dom_window_get_page_x_offset = (function(self)
                                           {
                                             return $hs_int(self["pageXOffset"]);
                                           });
var webkit_dom_dom_window_get_page_y_offset;
webkit_dom_dom_window_get_page_y_offset = (function(self)
                                           {
                                             return $hs_int(self["pageYOffset"]);
                                           });
var webkit_dom_dom_window_get_closed;
webkit_dom_dom_window_get_closed = (function(self)
                                    {
                                      return $hs_int((self["closed"] ? 1 : 0));
                                    });
var webkit_dom_dom_window_get_length;
webkit_dom_dom_window_get_length = (function(self)
                                    {
                                      return $hs_int(self["length"]);
                                    });
var webkit_dom_dom_window_set_name;
webkit_dom_dom_window_set_name = (function(self, val)
                                  {
                                    self["name"] = $hs_fromUtf8(val);
                                  });
var webkit_dom_dom_window_get_name;
webkit_dom_dom_window_get_name = (function(self)
                                  {
                                    return $hs_toUtf8(self["name"]);
                                  });
var webkit_dom_dom_window_set_status;
webkit_dom_dom_window_set_status = (function(self, val)
                                    {
                                      self["status"] = $hs_fromUtf8(val);
                                    });
var webkit_dom_dom_window_get_status;
webkit_dom_dom_window_get_status = (function(self)
                                    {
                                      return $hs_toUtf8(self["status"]);
                                    });
var webkit_dom_dom_window_set_default_status;
webkit_dom_dom_window_set_default_status = (function(self, val)
                                            {
                                              self["defaultStatus"] = $hs_fromUtf8(val);
                                            });
var webkit_dom_dom_window_get_default_status;
webkit_dom_dom_window_get_default_status = (function(self)
                                            {
                                              return $hs_toUtf8(self["defaultStatus"]);
                                            });
var webkit_dom_dom_window_get_self;
webkit_dom_dom_window_get_self = (function(self)
                                  {
                                    return self["self"];
                                  });
var webkit_dom_dom_window_get_window;
webkit_dom_dom_window_get_window = (function(self)
                                    {
                                      return self["window"];
                                    });
var webkit_dom_dom_window_get_frames;
webkit_dom_dom_window_get_frames = (function(self)
                                    {
                                      return self["frames"];
                                    });
var webkit_dom_dom_window_get_opener;
webkit_dom_dom_window_get_opener = (function(self)
                                    {
                                      return self["opener"];
                                    });
var webkit_dom_dom_window_get_parent;
webkit_dom_dom_window_get_parent = (function(self)
                                    {
                                      return self["parent"];
                                    });
var webkit_dom_dom_window_get_top;
webkit_dom_dom_window_get_top = (function(self)
                                 {
                                   return self["top"];
                                 });
var webkit_dom_dom_window_get_document;
webkit_dom_dom_window_get_document = (function(self)
                                      {
                                        return self["document"];
                                      });
var webkit_dom_dom_window_get_style_media;
webkit_dom_dom_window_get_style_media = (function(self)
                                         {
                                           return self["styleMedia"];
                                         });
var webkit_dom_dom_window_get_device_pixel_ratio;
webkit_dom_dom_window_get_device_pixel_ratio = (function(self)
                                                {
                                                  return self["devicePixelRatio"];
                                                });
var webkit_dom_dom_window_get_application_cache;
webkit_dom_dom_window_get_application_cache = (function(self)
                                               {
                                                 return self["applicationCache"];
                                               });
var webkit_dom_dom_window_get_session_storage;
webkit_dom_dom_window_get_session_storage = (function(self)
                                             {
                                               return self["sessionStorage"];
                                             });
var webkit_dom_dom_window_get_local_storage;
webkit_dom_dom_window_get_local_storage = (function(self)
                                           {
                                             return self["localStorage"];
                                           });
var webkit_dom_dom_window_get_console;
webkit_dom_dom_window_get_console = (function(self)
                                     {
                                       return self["console"];
                                     });
var webkit_dom_dom_window_set_onabort;
webkit_dom_dom_window_set_onabort = (function(self, val)
                                     {
                                       self["onabort"] = val;
                                     });
var webkit_dom_dom_window_get_onabort;
webkit_dom_dom_window_get_onabort = (function(self)
                                     {
                                       return self["onabort"];
                                     });
var webkit_dom_dom_window_set_onbeforeunload;
webkit_dom_dom_window_set_onbeforeunload = (function(self, val)
                                            {
                                              self["onbeforeunload"] = val;
                                            });
var webkit_dom_dom_window_get_onbeforeunload;
webkit_dom_dom_window_get_onbeforeunload = (function(self)
                                            {
                                              return self["onbeforeunload"];
                                            });
var webkit_dom_dom_window_set_onblur;
webkit_dom_dom_window_set_onblur = (function(self, val)
                                    {
                                      self["onblur"] = val;
                                    });
var webkit_dom_dom_window_get_onblur;
webkit_dom_dom_window_get_onblur = (function(self)
                                    {
                                      return self["onblur"];
                                    });
var webkit_dom_dom_window_set_oncanplay;
webkit_dom_dom_window_set_oncanplay = (function(self, val)
                                       {
                                         self["oncanplay"] = val;
                                       });
var webkit_dom_dom_window_get_oncanplay;
webkit_dom_dom_window_get_oncanplay = (function(self)
                                       {
                                         return self["oncanplay"];
                                       });
var webkit_dom_dom_window_set_oncanplaythrough;
webkit_dom_dom_window_set_oncanplaythrough = (function(self, val)
                                              {
                                                self["oncanplaythrough"] = val;
                                              });
var webkit_dom_dom_window_get_oncanplaythrough;
webkit_dom_dom_window_get_oncanplaythrough = (function(self)
                                              {
                                                return self["oncanplaythrough"];
                                              });
var webkit_dom_dom_window_set_onchange;
webkit_dom_dom_window_set_onchange = (function(self, val)
                                      {
                                        self["onchange"] = val;
                                      });
var webkit_dom_dom_window_get_onchange;
webkit_dom_dom_window_get_onchange = (function(self)
                                      {
                                        return self["onchange"];
                                      });
var webkit_dom_dom_window_set_onclick;
webkit_dom_dom_window_set_onclick = (function(self, val)
                                     {
                                       self["onclick"] = val;
                                     });
var webkit_dom_dom_window_get_onclick;
webkit_dom_dom_window_get_onclick = (function(self)
                                     {
                                       return self["onclick"];
                                     });
var webkit_dom_dom_window_set_oncontextmenu;
webkit_dom_dom_window_set_oncontextmenu = (function(self, val)
                                           {
                                             self["oncontextmenu"] = val;
                                           });
var webkit_dom_dom_window_get_oncontextmenu;
webkit_dom_dom_window_get_oncontextmenu = (function(self)
                                           {
                                             return self["oncontextmenu"];
                                           });
var webkit_dom_dom_window_set_ondblclick;
webkit_dom_dom_window_set_ondblclick = (function(self, val)
                                        {
                                          self["ondblclick"] = val;
                                        });
var webkit_dom_dom_window_get_ondblclick;
webkit_dom_dom_window_get_ondblclick = (function(self)
                                        {
                                          return self["ondblclick"];
                                        });
var webkit_dom_dom_window_set_ondrag;
webkit_dom_dom_window_set_ondrag = (function(self, val)
                                    {
                                      self["ondrag"] = val;
                                    });
var webkit_dom_dom_window_get_ondrag;
webkit_dom_dom_window_get_ondrag = (function(self)
                                    {
                                      return self["ondrag"];
                                    });
var webkit_dom_dom_window_set_ondragend;
webkit_dom_dom_window_set_ondragend = (function(self, val)
                                       {
                                         self["ondragend"] = val;
                                       });
var webkit_dom_dom_window_get_ondragend;
webkit_dom_dom_window_get_ondragend = (function(self)
                                       {
                                         return self["ondragend"];
                                       });
var webkit_dom_dom_window_set_ondragenter;
webkit_dom_dom_window_set_ondragenter = (function(self, val)
                                         {
                                           self["ondragenter"] = val;
                                         });
var webkit_dom_dom_window_get_ondragenter;
webkit_dom_dom_window_get_ondragenter = (function(self)
                                         {
                                           return self["ondragenter"];
                                         });
var webkit_dom_dom_window_set_ondragleave;
webkit_dom_dom_window_set_ondragleave = (function(self, val)
                                         {
                                           self["ondragleave"] = val;
                                         });
var webkit_dom_dom_window_get_ondragleave;
webkit_dom_dom_window_get_ondragleave = (function(self)
                                         {
                                           return self["ondragleave"];
                                         });
var webkit_dom_dom_window_set_ondragover;
webkit_dom_dom_window_set_ondragover = (function(self, val)
                                        {
                                          self["ondragover"] = val;
                                        });
var webkit_dom_dom_window_get_ondragover;
webkit_dom_dom_window_get_ondragover = (function(self)
                                        {
                                          return self["ondragover"];
                                        });
var webkit_dom_dom_window_set_ondragstart;
webkit_dom_dom_window_set_ondragstart = (function(self, val)
                                         {
                                           self["ondragstart"] = val;
                                         });
var webkit_dom_dom_window_get_ondragstart;
webkit_dom_dom_window_get_ondragstart = (function(self)
                                         {
                                           return self["ondragstart"];
                                         });
var webkit_dom_dom_window_set_ondrop;
webkit_dom_dom_window_set_ondrop = (function(self, val)
                                    {
                                      self["ondrop"] = val;
                                    });
var webkit_dom_dom_window_get_ondrop;
webkit_dom_dom_window_get_ondrop = (function(self)
                                    {
                                      return self["ondrop"];
                                    });
var webkit_dom_dom_window_set_ondurationchange;
webkit_dom_dom_window_set_ondurationchange = (function(self, val)
                                              {
                                                self["ondurationchange"] = val;
                                              });
var webkit_dom_dom_window_get_ondurationchange;
webkit_dom_dom_window_get_ondurationchange = (function(self)
                                              {
                                                return self["ondurationchange"];
                                              });
var webkit_dom_dom_window_set_onemptied;
webkit_dom_dom_window_set_onemptied = (function(self, val)
                                       {
                                         self["onemptied"] = val;
                                       });
var webkit_dom_dom_window_get_onemptied;
webkit_dom_dom_window_get_onemptied = (function(self)
                                       {
                                         return self["onemptied"];
                                       });
var webkit_dom_dom_window_set_onended;
webkit_dom_dom_window_set_onended = (function(self, val)
                                     {
                                       self["onended"] = val;
                                     });
var webkit_dom_dom_window_get_onended;
webkit_dom_dom_window_get_onended = (function(self)
                                     {
                                       return self["onended"];
                                     });
var webkit_dom_dom_window_set_onerror;
webkit_dom_dom_window_set_onerror = (function(self, val)
                                     {
                                       self["onerror"] = val;
                                     });
var webkit_dom_dom_window_get_onerror;
webkit_dom_dom_window_get_onerror = (function(self)
                                     {
                                       return self["onerror"];
                                     });
var webkit_dom_dom_window_set_onfocus;
webkit_dom_dom_window_set_onfocus = (function(self, val)
                                     {
                                       self["onfocus"] = val;
                                     });
var webkit_dom_dom_window_get_onfocus;
webkit_dom_dom_window_get_onfocus = (function(self)
                                     {
                                       return self["onfocus"];
                                     });
var webkit_dom_dom_window_set_onhashchange;
webkit_dom_dom_window_set_onhashchange = (function(self, val)
                                          {
                                            self["onhashchange"] = val;
                                          });
var webkit_dom_dom_window_get_onhashchange;
webkit_dom_dom_window_get_onhashchange = (function(self)
                                          {
                                            return self["onhashchange"];
                                          });
var webkit_dom_dom_window_set_oninput;
webkit_dom_dom_window_set_oninput = (function(self, val)
                                     {
                                       self["oninput"] = val;
                                     });
var webkit_dom_dom_window_get_oninput;
webkit_dom_dom_window_get_oninput = (function(self)
                                     {
                                       return self["oninput"];
                                     });
var webkit_dom_dom_window_set_oninvalid;
webkit_dom_dom_window_set_oninvalid = (function(self, val)
                                       {
                                         self["oninvalid"] = val;
                                       });
var webkit_dom_dom_window_get_oninvalid;
webkit_dom_dom_window_get_oninvalid = (function(self)
                                       {
                                         return self["oninvalid"];
                                       });
var webkit_dom_dom_window_set_onkeydown;
webkit_dom_dom_window_set_onkeydown = (function(self, val)
                                       {
                                         self["onkeydown"] = val;
                                       });
var webkit_dom_dom_window_get_onkeydown;
webkit_dom_dom_window_get_onkeydown = (function(self)
                                       {
                                         return self["onkeydown"];
                                       });
var webkit_dom_dom_window_set_onkeypress;
webkit_dom_dom_window_set_onkeypress = (function(self, val)
                                        {
                                          self["onkeypress"] = val;
                                        });
var webkit_dom_dom_window_get_onkeypress;
webkit_dom_dom_window_get_onkeypress = (function(self)
                                        {
                                          return self["onkeypress"];
                                        });
var webkit_dom_dom_window_set_onkeyup;
webkit_dom_dom_window_set_onkeyup = (function(self, val)
                                     {
                                       self["onkeyup"] = val;
                                     });
var webkit_dom_dom_window_get_onkeyup;
webkit_dom_dom_window_get_onkeyup = (function(self)
                                     {
                                       return self["onkeyup"];
                                     });
var webkit_dom_dom_window_set_onload;
webkit_dom_dom_window_set_onload = (function(self, val)
                                    {
                                      self["onload"] = val;
                                    });
var webkit_dom_dom_window_get_onload;
webkit_dom_dom_window_get_onload = (function(self)
                                    {
                                      return self["onload"];
                                    });
var webkit_dom_dom_window_set_onloadeddata;
webkit_dom_dom_window_set_onloadeddata = (function(self, val)
                                          {
                                            self["onloadeddata"] = val;
                                          });
var webkit_dom_dom_window_get_onloadeddata;
webkit_dom_dom_window_get_onloadeddata = (function(self)
                                          {
                                            return self["onloadeddata"];
                                          });
var webkit_dom_dom_window_set_onloadedmetadata;
webkit_dom_dom_window_set_onloadedmetadata = (function(self, val)
                                              {
                                                self["onloadedmetadata"] = val;
                                              });
var webkit_dom_dom_window_get_onloadedmetadata;
webkit_dom_dom_window_get_onloadedmetadata = (function(self)
                                              {
                                                return self["onloadedmetadata"];
                                              });
var webkit_dom_dom_window_set_onloadstart;
webkit_dom_dom_window_set_onloadstart = (function(self, val)
                                         {
                                           self["onloadstart"] = val;
                                         });
var webkit_dom_dom_window_get_onloadstart;
webkit_dom_dom_window_get_onloadstart = (function(self)
                                         {
                                           return self["onloadstart"];
                                         });
var webkit_dom_dom_window_set_onmessage;
webkit_dom_dom_window_set_onmessage = (function(self, val)
                                       {
                                         self["onmessage"] = val;
                                       });
var webkit_dom_dom_window_get_onmessage;
webkit_dom_dom_window_get_onmessage = (function(self)
                                       {
                                         return self["onmessage"];
                                       });
var webkit_dom_dom_window_set_onmousedown;
webkit_dom_dom_window_set_onmousedown = (function(self, val)
                                         {
                                           self["onmousedown"] = val;
                                         });
var webkit_dom_dom_window_get_onmousedown;
webkit_dom_dom_window_get_onmousedown = (function(self)
                                         {
                                           return self["onmousedown"];
                                         });
var webkit_dom_dom_window_set_onmousemove;
webkit_dom_dom_window_set_onmousemove = (function(self, val)
                                         {
                                           self["onmousemove"] = val;
                                         });
var webkit_dom_dom_window_get_onmousemove;
webkit_dom_dom_window_get_onmousemove = (function(self)
                                         {
                                           return self["onmousemove"];
                                         });
var webkit_dom_dom_window_set_onmouseout;
webkit_dom_dom_window_set_onmouseout = (function(self, val)
                                        {
                                          self["onmouseout"] = val;
                                        });
var webkit_dom_dom_window_get_onmouseout;
webkit_dom_dom_window_get_onmouseout = (function(self)
                                        {
                                          return self["onmouseout"];
                                        });
var webkit_dom_dom_window_set_onmouseover;
webkit_dom_dom_window_set_onmouseover = (function(self, val)
                                         {
                                           self["onmouseover"] = val;
                                         });
var webkit_dom_dom_window_get_onmouseover;
webkit_dom_dom_window_get_onmouseover = (function(self)
                                         {
                                           return self["onmouseover"];
                                         });
var webkit_dom_dom_window_set_onmouseup;
webkit_dom_dom_window_set_onmouseup = (function(self, val)
                                       {
                                         self["onmouseup"] = val;
                                       });
var webkit_dom_dom_window_get_onmouseup;
webkit_dom_dom_window_get_onmouseup = (function(self)
                                       {
                                         return self["onmouseup"];
                                       });
var webkit_dom_dom_window_set_onmousewheel;
webkit_dom_dom_window_set_onmousewheel = (function(self, val)
                                          {
                                            self["onmousewheel"] = val;
                                          });
var webkit_dom_dom_window_get_onmousewheel;
webkit_dom_dom_window_get_onmousewheel = (function(self)
                                          {
                                            return self["onmousewheel"];
                                          });
var webkit_dom_dom_window_set_onoffline;
webkit_dom_dom_window_set_onoffline = (function(self, val)
                                       {
                                         self["onoffline"] = val;
                                       });
var webkit_dom_dom_window_get_onoffline;
webkit_dom_dom_window_get_onoffline = (function(self)
                                       {
                                         return self["onoffline"];
                                       });
var webkit_dom_dom_window_set_ononline;
webkit_dom_dom_window_set_ononline = (function(self, val)
                                      {
                                        self["ononline"] = val;
                                      });
var webkit_dom_dom_window_get_ononline;
webkit_dom_dom_window_get_ononline = (function(self)
                                      {
                                        return self["ononline"];
                                      });
var webkit_dom_dom_window_set_onpagehide;
webkit_dom_dom_window_set_onpagehide = (function(self, val)
                                        {
                                          self["onpagehide"] = val;
                                        });
var webkit_dom_dom_window_get_onpagehide;
webkit_dom_dom_window_get_onpagehide = (function(self)
                                        {
                                          return self["onpagehide"];
                                        });
var webkit_dom_dom_window_set_onpageshow;
webkit_dom_dom_window_set_onpageshow = (function(self, val)
                                        {
                                          self["onpageshow"] = val;
                                        });
var webkit_dom_dom_window_get_onpageshow;
webkit_dom_dom_window_get_onpageshow = (function(self)
                                        {
                                          return self["onpageshow"];
                                        });
var webkit_dom_dom_window_set_onpause;
webkit_dom_dom_window_set_onpause = (function(self, val)
                                     {
                                       self["onpause"] = val;
                                     });
var webkit_dom_dom_window_get_onpause;
webkit_dom_dom_window_get_onpause = (function(self)
                                     {
                                       return self["onpause"];
                                     });
var webkit_dom_dom_window_set_onplay;
webkit_dom_dom_window_set_onplay = (function(self, val)
                                    {
                                      self["onplay"] = val;
                                    });
var webkit_dom_dom_window_get_onplay;
webkit_dom_dom_window_get_onplay = (function(self)
                                    {
                                      return self["onplay"];
                                    });
var webkit_dom_dom_window_set_onplaying;
webkit_dom_dom_window_set_onplaying = (function(self, val)
                                       {
                                         self["onplaying"] = val;
                                       });
var webkit_dom_dom_window_get_onplaying;
webkit_dom_dom_window_get_onplaying = (function(self)
                                       {
                                         return self["onplaying"];
                                       });
var webkit_dom_dom_window_set_onpopstate;
webkit_dom_dom_window_set_onpopstate = (function(self, val)
                                        {
                                          self["onpopstate"] = val;
                                        });
var webkit_dom_dom_window_get_onpopstate;
webkit_dom_dom_window_get_onpopstate = (function(self)
                                        {
                                          return self["onpopstate"];
                                        });
var webkit_dom_dom_window_set_onprogress;
webkit_dom_dom_window_set_onprogress = (function(self, val)
                                        {
                                          self["onprogress"] = val;
                                        });
var webkit_dom_dom_window_get_onprogress;
webkit_dom_dom_window_get_onprogress = (function(self)
                                        {
                                          return self["onprogress"];
                                        });
var webkit_dom_dom_window_set_onratechange;
webkit_dom_dom_window_set_onratechange = (function(self, val)
                                          {
                                            self["onratechange"] = val;
                                          });
var webkit_dom_dom_window_get_onratechange;
webkit_dom_dom_window_get_onratechange = (function(self)
                                          {
                                            return self["onratechange"];
                                          });
var webkit_dom_dom_window_set_onresize;
webkit_dom_dom_window_set_onresize = (function(self, val)
                                      {
                                        self["onresize"] = val;
                                      });
var webkit_dom_dom_window_get_onresize;
webkit_dom_dom_window_get_onresize = (function(self)
                                      {
                                        return self["onresize"];
                                      });
var webkit_dom_dom_window_set_onscroll;
webkit_dom_dom_window_set_onscroll = (function(self, val)
                                      {
                                        self["onscroll"] = val;
                                      });
var webkit_dom_dom_window_get_onscroll;
webkit_dom_dom_window_get_onscroll = (function(self)
                                      {
                                        return self["onscroll"];
                                      });
var webkit_dom_dom_window_set_onseeked;
webkit_dom_dom_window_set_onseeked = (function(self, val)
                                      {
                                        self["onseeked"] = val;
                                      });
var webkit_dom_dom_window_get_onseeked;
webkit_dom_dom_window_get_onseeked = (function(self)
                                      {
                                        return self["onseeked"];
                                      });
var webkit_dom_dom_window_set_onseeking;
webkit_dom_dom_window_set_onseeking = (function(self, val)
                                       {
                                         self["onseeking"] = val;
                                       });
var webkit_dom_dom_window_get_onseeking;
webkit_dom_dom_window_get_onseeking = (function(self)
                                       {
                                         return self["onseeking"];
                                       });
var webkit_dom_dom_window_set_onselect;
webkit_dom_dom_window_set_onselect = (function(self, val)
                                      {
                                        self["onselect"] = val;
                                      });
var webkit_dom_dom_window_get_onselect;
webkit_dom_dom_window_get_onselect = (function(self)
                                      {
                                        return self["onselect"];
                                      });
var webkit_dom_dom_window_set_onstalled;
webkit_dom_dom_window_set_onstalled = (function(self, val)
                                       {
                                         self["onstalled"] = val;
                                       });
var webkit_dom_dom_window_get_onstalled;
webkit_dom_dom_window_get_onstalled = (function(self)
                                       {
                                         return self["onstalled"];
                                       });
var webkit_dom_dom_window_set_onstorage;
webkit_dom_dom_window_set_onstorage = (function(self, val)
                                       {
                                         self["onstorage"] = val;
                                       });
var webkit_dom_dom_window_get_onstorage;
webkit_dom_dom_window_get_onstorage = (function(self)
                                       {
                                         return self["onstorage"];
                                       });
var webkit_dom_dom_window_set_onsubmit;
webkit_dom_dom_window_set_onsubmit = (function(self, val)
                                      {
                                        self["onsubmit"] = val;
                                      });
var webkit_dom_dom_window_get_onsubmit;
webkit_dom_dom_window_get_onsubmit = (function(self)
                                      {
                                        return self["onsubmit"];
                                      });
var webkit_dom_dom_window_set_onsuspend;
webkit_dom_dom_window_set_onsuspend = (function(self, val)
                                       {
                                         self["onsuspend"] = val;
                                       });
var webkit_dom_dom_window_get_onsuspend;
webkit_dom_dom_window_get_onsuspend = (function(self)
                                       {
                                         return self["onsuspend"];
                                       });
var webkit_dom_dom_window_set_ontimeupdate;
webkit_dom_dom_window_set_ontimeupdate = (function(self, val)
                                          {
                                            self["ontimeupdate"] = val;
                                          });
var webkit_dom_dom_window_get_ontimeupdate;
webkit_dom_dom_window_get_ontimeupdate = (function(self)
                                          {
                                            return self["ontimeupdate"];
                                          });
var webkit_dom_dom_window_set_onunload;
webkit_dom_dom_window_set_onunload = (function(self, val)
                                      {
                                        self["onunload"] = val;
                                      });
var webkit_dom_dom_window_get_onunload;
webkit_dom_dom_window_get_onunload = (function(self)
                                      {
                                        return self["onunload"];
                                      });
var webkit_dom_dom_window_set_onvolumechange;
webkit_dom_dom_window_set_onvolumechange = (function(self, val)
                                            {
                                              self["onvolumechange"] = val;
                                            });
var webkit_dom_dom_window_get_onvolumechange;
webkit_dom_dom_window_get_onvolumechange = (function(self)
                                            {
                                              return self["onvolumechange"];
                                            });
var webkit_dom_dom_window_set_onwaiting;
webkit_dom_dom_window_set_onwaiting = (function(self, val)
                                       {
                                         self["onwaiting"] = val;
                                       });
var webkit_dom_dom_window_get_onwaiting;
webkit_dom_dom_window_get_onwaiting = (function(self)
                                       {
                                         return self["onwaiting"];
                                       });
var webkit_dom_dom_window_set_onreset;
webkit_dom_dom_window_set_onreset = (function(self, val)
                                     {
                                       self["onreset"] = val;
                                     });
var webkit_dom_dom_window_get_onreset;
webkit_dom_dom_window_get_onreset = (function(self)
                                     {
                                       return self["onreset"];
                                     });
var webkit_dom_dom_window_set_onsearch;
webkit_dom_dom_window_set_onsearch = (function(self, val)
                                      {
                                        self["onsearch"] = val;
                                      });
var webkit_dom_dom_window_get_onsearch;
webkit_dom_dom_window_get_onsearch = (function(self)
                                      {
                                        return self["onsearch"];
                                      });
var webkit_dom_dom_window_set_onwebkitanimationend;
webkit_dom_dom_window_set_onwebkitanimationend = (function(self,
                                                           val)
                                                  {
                                                    self["onwebkitanimationend"] = val;
                                                  });
var webkit_dom_dom_window_get_onwebkitanimationend;
webkit_dom_dom_window_get_onwebkitanimationend = (function(self)
                                                  {
                                                    return self["onwebkitanimationend"];
                                                  });
var webkit_dom_dom_window_set_onwebkitanimationiteration;
webkit_dom_dom_window_set_onwebkitanimationiteration = (function(self,
                                                                 val)
                                                        {
                                                          self["onwebkitanimationiteration"] = val;
                                                        });
var webkit_dom_dom_window_get_onwebkitanimationiteration;
webkit_dom_dom_window_get_onwebkitanimationiteration = (function(self)
                                                        {
                                                          return self["onwebkitanimationiteration"];
                                                        });
var webkit_dom_dom_window_set_onwebkitanimationstart;
webkit_dom_dom_window_set_onwebkitanimationstart = (function(self,
                                                             val)
                                                    {
                                                      self["onwebkitanimationstart"] = val;
                                                    });
var webkit_dom_dom_window_get_onwebkitanimationstart;
webkit_dom_dom_window_get_onwebkitanimationstart = (function(self)
                                                    {
                                                      return self["onwebkitanimationstart"];
                                                    });
var webkit_dom_dom_window_set_onwebkittransitionend;
webkit_dom_dom_window_set_onwebkittransitionend = (function(self,
                                                            val)
                                                   {
                                                     self["onwebkittransitionend"] = val;
                                                   });
var webkit_dom_dom_window_get_onwebkittransitionend;
webkit_dom_dom_window_get_onwebkittransitionend = (function(self)
                                                   {
                                                     return self["onwebkittransitionend"];
                                                   });
var webkit_dom_dom_window_set_ontouchstart;
webkit_dom_dom_window_set_ontouchstart = (function(self, val)
                                          {
                                            self["ontouchstart"] = val;
                                          });
var webkit_dom_dom_window_get_ontouchstart;
webkit_dom_dom_window_get_ontouchstart = (function(self)
                                          {
                                            return self["ontouchstart"];
                                          });
var webkit_dom_dom_window_set_ontouchmove;
webkit_dom_dom_window_set_ontouchmove = (function(self, val)
                                         {
                                           self["ontouchmove"] = val;
                                         });
var webkit_dom_dom_window_get_ontouchmove;
webkit_dom_dom_window_get_ontouchmove = (function(self)
                                         {
                                           return self["ontouchmove"];
                                         });
var webkit_dom_dom_window_set_ontouchend;
webkit_dom_dom_window_set_ontouchend = (function(self, val)
                                        {
                                          self["ontouchend"] = val;
                                        });
var webkit_dom_dom_window_get_ontouchend;
webkit_dom_dom_window_get_ontouchend = (function(self)
                                        {
                                          return self["ontouchend"];
                                        });
var webkit_dom_dom_window_set_ontouchcancel;
webkit_dom_dom_window_set_ontouchcancel = (function(self, val)
                                           {
                                             self["ontouchcancel"] = val;
                                           });
var webkit_dom_dom_window_get_ontouchcancel;
webkit_dom_dom_window_get_ontouchcancel = (function(self)
                                           {
                                             return self["ontouchcancel"];
                                           });
var webkit_dom_dom_window_set_ondevicemotion;
webkit_dom_dom_window_set_ondevicemotion = (function(self, val)
                                            {
                                              self["ondevicemotion"] = val;
                                            });
var webkit_dom_dom_window_get_ondevicemotion;
webkit_dom_dom_window_get_ondevicemotion = (function(self)
                                            {
                                              return self["ondevicemotion"];
                                            });
var webkit_dom_dom_window_set_ondeviceorientation;
webkit_dom_dom_window_set_ondeviceorientation = (function(self,
                                                          val)
                                                 {
                                                   self["ondeviceorientation"] = val;
                                                 });
var webkit_dom_dom_window_get_ondeviceorientation;
webkit_dom_dom_window_get_ondeviceorientation = (function(self)
                                                 {
                                                   return self["ondeviceorientation"];
                                                 });
// Graphics.UI.Gtk.WebKit.DOM.Core
webkit_dom_dom_token_list_get_type = (function()
                                      {
                                        return DOMTokenList;
                                      });
var webkit_dom_dom_token_list_item;
webkit_dom_dom_token_list_item = (function(self, index)
                                  {
                                    return $hs_toUtf8(self["item"]($hs_intToNumber(index)));
                                  });
var webkit_dom_dom_token_list_contains;
webkit_dom_dom_token_list_contains = (function(self, token)
                                      {
                                        return $hs_int((self["contains"]($hs_fromUtf8(token)) ? 1 : 0));
                                      });
var webkit_dom_dom_token_list_add;
webkit_dom_dom_token_list_add = (function(self, token)
                                 {
                                   return self["add"]($hs_fromUtf8(token));
                                 });
var webkit_dom_dom_token_list_remove;
webkit_dom_dom_token_list_remove = (function(self, token)
                                    {
                                      return self["remove"]($hs_fromUtf8(token));
                                    });
var webkit_dom_dom_token_list_toggle;
webkit_dom_dom_token_list_toggle = (function(self, token)
                                    {
                                      return $hs_int((self["toggle"]($hs_fromUtf8(token)) ? 1 : 0));
                                    });
var webkit_dom_dom_token_list_get_length;
webkit_dom_dom_token_list_get_length = (function(self)
                                        {
                                          return $hs_int(self["length"]);
                                        });
// Graphics.UI.Gtk.WebKit.DOM.Core
webkit_dom_dom_settable_token_list_get_type = (function()
                                               {
                                                 return DOMSettableTokenList;
                                               });
var webkit_dom_dom_settable_token_list_set_value;
webkit_dom_dom_settable_token_list_set_value = (function(self, val)
                                                {
                                                  self["value"] = $hs_fromUtf8(val);
                                                });
var webkit_dom_dom_settable_token_list_get_value;
webkit_dom_dom_settable_token_list_get_value = (function(self)
                                                {
                                                  return $hs_toUtf8(self["value"]);
                                                });
// Graphics.UI.Gtk.WebKit.DOM.Window
webkit_dom_dom_selection_get_type = (function()
                                     {
                                       return DOMSelection;
                                     });
var webkit_dom_dom_selection_collapse;
webkit_dom_dom_selection_collapse = (function(self, node, index)
                                     {
                                       return self["collapse"](node, $hs_intToNumber(index));
                                     });
var webkit_dom_dom_selection_collapse_to_end;
webkit_dom_dom_selection_collapse_to_end = (function(self)
                                            {
                                              return self["collapseToEnd"]();
                                            });
var webkit_dom_dom_selection_collapse_to_start;
webkit_dom_dom_selection_collapse_to_start = (function(self)
                                              {
                                                return self["collapseToStart"]();
                                              });
var webkit_dom_dom_selection_delete_from_document;
webkit_dom_dom_selection_delete_from_document = (function(self)
                                                 {
                                                   return self["deleteFromDocument"]();
                                                 });
var webkit_dom_dom_selection_contains_node;
webkit_dom_dom_selection_contains_node = (function(self, node,
                                                   allowPartial)
                                          {
                                            return $hs_int((self["containsNode"](node,
                                                                                 ($hs_intToNumber(allowPartial)
                                                                                  !=
                                                                                  0)) ? 1 : 0));
                                          });
var webkit_dom_dom_selection_select_all_children;
webkit_dom_dom_selection_select_all_children = (function(self,
                                                         node)
                                                {
                                                  return self["selectAllChildren"](node);
                                                });
var webkit_dom_dom_selection_extend;
webkit_dom_dom_selection_extend = (function(self, node, offset)
                                   {
                                     return self["extend"](node, $hs_intToNumber(offset));
                                   });
var webkit_dom_dom_selection_get_range_at;
webkit_dom_dom_selection_get_range_at = (function(self, index)
                                         {
                                           return self["getRangeAt"]($hs_intToNumber(index));
                                         });
var webkit_dom_dom_selection_remove_all_ranges;
webkit_dom_dom_selection_remove_all_ranges = (function(self)
                                              {
                                                return self["removeAllRanges"]();
                                              });
var webkit_dom_dom_selection_add_range;
webkit_dom_dom_selection_add_range = (function(self, range)
                                      {
                                        return self["addRange"](range);
                                      });
var webkit_dom_dom_selection_modify;
webkit_dom_dom_selection_modify = (function(self, alter, direction,
                                            granularity)
                                   {
                                     return self["modify"]($hs_fromUtf8(alter),
                                                           $hs_fromUtf8(direction),
                                                           $hs_fromUtf8(granularity));
                                   });
var webkit_dom_dom_selection_set_base_and_extent;
webkit_dom_dom_selection_set_base_and_extent = (function(self,
                                                         baseNode, baseOffset, extentNode,
                                                         extentOffset)
                                                {
                                                  return self["setBaseAndExtent"](baseNode,
                                                                                  $hs_intToNumber(baseOffset),
                                                                                  extentNode,
                                                                                  $hs_intToNumber(extentOffset));
                                                });
var webkit_dom_dom_selection_set_position;
webkit_dom_dom_selection_set_position = (function(self, node,
                                                  offset)
                                         {
                                           return self["setPosition"](node,
                                                                      $hs_intToNumber(offset));
                                         });
var webkit_dom_dom_selection_empty;
webkit_dom_dom_selection_empty = (function(self)
                                  {
                                    return self["empty"]();
                                  });
var webkit_dom_dom_selection_get_anchor_node;
webkit_dom_dom_selection_get_anchor_node = (function(self)
                                            {
                                              return self["anchorNode"];
                                            });
var webkit_dom_dom_selection_get_anchor_offset;
webkit_dom_dom_selection_get_anchor_offset = (function(self)
                                              {
                                                return $hs_int(self["anchorOffset"]);
                                              });
var webkit_dom_dom_selection_get_focus_node;
webkit_dom_dom_selection_get_focus_node = (function(self)
                                           {
                                             return self["focusNode"];
                                           });
var webkit_dom_dom_selection_get_focus_offset;
webkit_dom_dom_selection_get_focus_offset = (function(self)
                                             {
                                               return $hs_int(self["focusOffset"]);
                                             });
var webkit_dom_dom_selection_get_is_collapsed;
webkit_dom_dom_selection_get_is_collapsed = (function(self)
                                             {
                                               return $hs_int((self["isCollapsed"] ? 1 : 0));
                                             });
var webkit_dom_dom_selection_get_range_count;
webkit_dom_dom_selection_get_range_count = (function(self)
                                            {
                                              return $hs_int(self["rangeCount"]);
                                            });
var webkit_dom_dom_selection_get_base_node;
webkit_dom_dom_selection_get_base_node = (function(self)
                                          {
                                            return self["baseNode"];
                                          });
var webkit_dom_dom_selection_get_base_offset;
webkit_dom_dom_selection_get_base_offset = (function(self)
                                            {
                                              return $hs_int(self["baseOffset"]);
                                            });
var webkit_dom_dom_selection_get_extent_node;
webkit_dom_dom_selection_get_extent_node = (function(self)
                                            {
                                              return self["extentNode"];
                                            });
var webkit_dom_dom_selection_get_extent_offset;
webkit_dom_dom_selection_get_extent_offset = (function(self)
                                              {
                                                return $hs_int(self["extentOffset"]);
                                              });
// Graphics.UI.Gtk.WebKit.DOM.Window
webkit_dom_dom_plugin_array_get_type = (function()
                                        {
                                          return DOMPluginArray;
                                        });
var webkit_dom_dom_plugin_array_item;
webkit_dom_dom_plugin_array_item = (function(self, index)
                                    {
                                      return self["item"]($hs_intToNumber(index));
                                    });
var webkit_dom_dom_plugin_array_named_item;
webkit_dom_dom_plugin_array_named_item = (function(self, name)
                                          {
                                            return self["namedItem"]($hs_fromUtf8(name));
                                          });
var webkit_dom_dom_plugin_array_refresh;
webkit_dom_dom_plugin_array_refresh = (function(self, reload)
                                       {
                                         return self["refresh"](($hs_intToNumber(reload) != 0));
                                       });
var webkit_dom_dom_plugin_array_get_length;
webkit_dom_dom_plugin_array_get_length = (function(self)
                                          {
                                            return $hs_int(self["length"]);
                                          });
// Graphics.UI.Gtk.WebKit.DOM.Window
webkit_dom_dom_plugin_get_type = (function()
                                  {
                                    return DOMPlugin;
                                  });
var webkit_dom_dom_plugin_item;
webkit_dom_dom_plugin_item = (function(self, index)
                              {
                                return self["item"]($hs_intToNumber(index));
                              });
var webkit_dom_dom_plugin_named_item;
webkit_dom_dom_plugin_named_item = (function(self, name)
                                    {
                                      return self["namedItem"]($hs_fromUtf8(name));
                                    });
var webkit_dom_dom_plugin_get_name;
webkit_dom_dom_plugin_get_name = (function(self)
                                  {
                                    return $hs_toUtf8(self["name"]);
                                  });
var webkit_dom_dom_plugin_get_filename;
webkit_dom_dom_plugin_get_filename = (function(self)
                                      {
                                        return $hs_toUtf8(self["filename"]);
                                      });
var webkit_dom_dom_plugin_get_description;
webkit_dom_dom_plugin_get_description = (function(self)
                                         {
                                           return $hs_toUtf8(self["description"]);
                                         });
var webkit_dom_dom_plugin_get_length;
webkit_dom_dom_plugin_get_length = (function(self)
                                    {
                                      return $hs_int(self["length"]);
                                    });
// Graphics.UI.Gtk.WebKit.DOM.Window
webkit_dom_dom_mime_type_array_get_type = (function()
                                           {
                                             return DOMMimeTypeArray;
                                           });
var webkit_dom_dom_mime_type_array_item;
webkit_dom_dom_mime_type_array_item = (function(self, index)
                                       {
                                         return self["item"]($hs_intToNumber(index));
                                       });
var webkit_dom_dom_mime_type_array_named_item;
webkit_dom_dom_mime_type_array_named_item = (function(self, name)
                                             {
                                               return self["namedItem"]($hs_fromUtf8(name));
                                             });
var webkit_dom_dom_mime_type_array_get_length;
webkit_dom_dom_mime_type_array_get_length = (function(self)
                                             {
                                               return $hs_int(self["length"]);
                                             });
// Graphics.UI.Gtk.WebKit.DOM.Window
webkit_dom_dom_mime_type_get_type = (function()
                                     {
                                       return DOMMimeType;
                                     });
var webkit_dom_dom_mime_type_get_suffixes;
webkit_dom_dom_mime_type_get_suffixes = (function(self)
                                         {
                                           return $hs_toUtf8(self["suffixes"]);
                                         });
var webkit_dom_dom_mime_type_get_description;
webkit_dom_dom_mime_type_get_description = (function(self)
                                            {
                                              return $hs_toUtf8(self["description"]);
                                            });
var webkit_dom_dom_mime_type_get_enabled_plugin;
webkit_dom_dom_mime_type_get_enabled_plugin = (function(self)
                                               {
                                                 return self["enabledPlugin"];
                                               });
// Graphics.UI.Gtk.WebKit.DOM.Core
webkit_dom_dom_implementation_get_type = (function()
                                          {
                                            return DOMImplementation;
                                          });
var webkit_dom_dom_implementation_has_feature;
webkit_dom_dom_implementation_has_feature = (function(self,
                                                      feature, version)
                                             {
                                               return $hs_int((self["hasFeature"]($hs_fromUtf8(feature),
                                                                                  $hs_fromUtf8(version)) ? 1 : 0));
                                             });
var webkit_dom_dom_implementation_create_document_type;
webkit_dom_dom_implementation_create_document_type = (function(self,
                                                               qualifiedName, publicId, systemId)
                                                      {
                                                        return self["createDocumentType"]($hs_fromUtf8(qualifiedName),
                                                                                          $hs_fromUtf8(publicId),
                                                                                          $hs_fromUtf8(systemId));
                                                      });
var webkit_dom_dom_implementation_create_document;
webkit_dom_dom_implementation_create_document = (function(self,
                                                          namespaceURI, qualifiedName, doctype)
                                                 {
                                                   return self["createDocument"]($hs_fromUtf8(namespaceURI),
                                                                                 $hs_fromUtf8(qualifiedName),
                                                                                 doctype);
                                                 });
var webkit_dom_dom_implementation_create_css_style_sheet;
webkit_dom_dom_implementation_create_css_style_sheet = (function(self,
                                                                 title, media)
                                                        {
                                                          return self["createCSSStyleSheet"]($hs_fromUtf8(title),
                                                                                             $hs_fromUtf8(media));
                                                        });
var webkit_dom_dom_implementation_create_html_document;
webkit_dom_dom_implementation_create_html_document = (function(self,
                                                               title)
                                                      {
                                                        return self["createHTMLDocument"]($hs_fromUtf8(title));
                                                      });
// Graphics.UI.Gtk.WebKit.DOM.Offline
webkit_dom_dom_application_cache_get_type = (function()
                                             {
                                               return DOMApplicationCache;
                                             });
var webkit_dom_dom_application_cache_update;
webkit_dom_dom_application_cache_update = (function(self)
                                           {
                                             return self["update"]();
                                           });
var webkit_dom_dom_application_cache_swap_cache;
webkit_dom_dom_application_cache_swap_cache = (function(self)
                                               {
                                                 return self["swapCache"]();
                                               });
var webkit_dom_dom_application_cache_abort;
webkit_dom_dom_application_cache_abort = (function(self)
                                          {
                                            return self["abort"]();
                                          });
var webkit_dom_dom_application_cache_dispatch_event;
webkit_dom_dom_application_cache_dispatch_event = (function(self,
                                                            evt)
                                                   {
                                                     return $hs_int((self["dispatchEvent"](evt) ? 1 : 0));
                                                   });
var webkit_dom_dom_application_cache_get_status;
webkit_dom_dom_application_cache_get_status = (function(self)
                                               {
                                                 return $hs_int(self["status"]);
                                               });
var webkit_dom_dom_application_cache_set_onchecking;
webkit_dom_dom_application_cache_set_onchecking = (function(self,
                                                            val)
                                                   {
                                                     self["onchecking"] = val;
                                                   });
var webkit_dom_dom_application_cache_get_onchecking;
webkit_dom_dom_application_cache_get_onchecking = (function(self)
                                                   {
                                                     return self["onchecking"];
                                                   });
var webkit_dom_dom_application_cache_set_onerror;
webkit_dom_dom_application_cache_set_onerror = (function(self, val)
                                                {
                                                  self["onerror"] = val;
                                                });
var webkit_dom_dom_application_cache_get_onerror;
webkit_dom_dom_application_cache_get_onerror = (function(self)
                                                {
                                                  return self["onerror"];
                                                });
var webkit_dom_dom_application_cache_set_onnoupdate;
webkit_dom_dom_application_cache_set_onnoupdate = (function(self,
                                                            val)
                                                   {
                                                     self["onnoupdate"] = val;
                                                   });
var webkit_dom_dom_application_cache_get_onnoupdate;
webkit_dom_dom_application_cache_get_onnoupdate = (function(self)
                                                   {
                                                     return self["onnoupdate"];
                                                   });
var webkit_dom_dom_application_cache_set_ondownloading;
webkit_dom_dom_application_cache_set_ondownloading = (function(self,
                                                               val)
                                                      {
                                                        self["ondownloading"] = val;
                                                      });
var webkit_dom_dom_application_cache_get_ondownloading;
webkit_dom_dom_application_cache_get_ondownloading = (function(self)
                                                      {
                                                        return self["ondownloading"];
                                                      });
var webkit_dom_dom_application_cache_set_onprogress;
webkit_dom_dom_application_cache_set_onprogress = (function(self,
                                                            val)
                                                   {
                                                     self["onprogress"] = val;
                                                   });
var webkit_dom_dom_application_cache_get_onprogress;
webkit_dom_dom_application_cache_get_onprogress = (function(self)
                                                   {
                                                     return self["onprogress"];
                                                   });
var webkit_dom_dom_application_cache_set_onupdateready;
webkit_dom_dom_application_cache_set_onupdateready = (function(self,
                                                               val)
                                                      {
                                                        self["onupdateready"] = val;
                                                      });
var webkit_dom_dom_application_cache_get_onupdateready;
webkit_dom_dom_application_cache_get_onupdateready = (function(self)
                                                      {
                                                        return self["onupdateready"];
                                                      });
var webkit_dom_dom_application_cache_set_oncached;
webkit_dom_dom_application_cache_set_oncached = (function(self,
                                                          val)
                                                 {
                                                   self["oncached"] = val;
                                                 });
var webkit_dom_dom_application_cache_get_oncached;
webkit_dom_dom_application_cache_get_oncached = (function(self)
                                                 {
                                                   return self["oncached"];
                                                 });
var webkit_dom_dom_application_cache_set_onobsolete;
webkit_dom_dom_application_cache_set_onobsolete = (function(self,
                                                            val)
                                                   {
                                                     self["onobsolete"] = val;
                                                   });
var webkit_dom_dom_application_cache_get_onobsolete;
webkit_dom_dom_application_cache_get_onobsolete = (function(self)
                                                   {
                                                     return self["onobsolete"];
                                                   });
// Graphics.UI.Gtk.WebKit.DOM.Core
webkit_dom_document_type_get_type = (function()
                                     {
                                       return DocumentType;
                                     });
var webkit_dom_document_type_get_name;
webkit_dom_document_type_get_name = (function(self)
                                     {
                                       return $hs_toUtf8(self["name"]);
                                     });
var webkit_dom_document_type_get_entities;
webkit_dom_document_type_get_entities = (function(self)
                                         {
                                           return self["entities"];
                                         });
var webkit_dom_document_type_get_notations;
webkit_dom_document_type_get_notations = (function(self)
                                          {
                                            return self["notations"];
                                          });
var webkit_dom_document_type_get_public_id;
webkit_dom_document_type_get_public_id = (function(self)
                                          {
                                            return $hs_toUtf8(self["publicId"]);
                                          });
var webkit_dom_document_type_get_system_id;
webkit_dom_document_type_get_system_id = (function(self)
                                          {
                                            return $hs_toUtf8(self["systemId"]);
                                          });
var webkit_dom_document_type_get_internal_subset;
webkit_dom_document_type_get_internal_subset = (function(self)
                                                {
                                                  return $hs_toUtf8(self["internalSubset"]);
                                                });
// Graphics.UI.Gtk.WebKit.DOM.Core
webkit_dom_document_fragment_get_type = (function()
                                         {
                                           return DocumentFragment;
                                         });
var webkit_dom_document_fragment_query_selector;
webkit_dom_document_fragment_query_selector = (function(self,
                                                        selectors)
                                               {
                                                 return self["querySelector"]($hs_fromUtf8(selectors));
                                               });
var webkit_dom_document_fragment_query_selector_all;
webkit_dom_document_fragment_query_selector_all = (function(self,
                                                            selectors)
                                                   {
                                                     return self["querySelectorAll"]($hs_fromUtf8(selectors));
                                                   });
// Graphics.UI.Gtk.WebKit.DOM.Core
webkit_dom_document_get_type = (function()
                                {
                                  return Document;
                                });
var webkit_dom_document_create_element;
webkit_dom_document_create_element = (function(self, tagName)
                                      {
                                        return self["createElement"]($hs_fromUtf8(tagName));
                                      });
var webkit_dom_document_create_document_fragment;
webkit_dom_document_create_document_fragment = (function(self)
                                                {
                                                  return self["createDocumentFragment"]();
                                                });
var webkit_dom_document_create_text_node;
webkit_dom_document_create_text_node = (function(self, data)
                                        {
                                          return self["createTextNode"]($hs_fromUtf8(data));
                                        });
var webkit_dom_document_create_comment;
webkit_dom_document_create_comment = (function(self, data)
                                      {
                                        return self["createComment"]($hs_fromUtf8(data));
                                      });
var webkit_dom_document_create_cdata_section;
webkit_dom_document_create_cdata_section = (function(self, data)
                                            {
                                              return self["createCDATASection"]($hs_fromUtf8(data));
                                            });
var webkit_dom_document_create_processing_instruction;
webkit_dom_document_create_processing_instruction = (function(self,
                                                              target, data)
                                                     {
                                                       return self["createProcessingInstruction"]($hs_fromUtf8(target),
                                                                                                  $hs_fromUtf8(data));
                                                     });
var webkit_dom_document_create_attribute;
webkit_dom_document_create_attribute = (function(self, name)
                                        {
                                          return self["createAttribute"]($hs_fromUtf8(name));
                                        });
var webkit_dom_document_create_entity_reference;
webkit_dom_document_create_entity_reference = (function(self, name)
                                               {
                                                 return self["createEntityReference"]($hs_fromUtf8(name));
                                               });
var webkit_dom_document_get_elements_by_tag_name;
webkit_dom_document_get_elements_by_tag_name = (function(self,
                                                         tagname)
                                                {
                                                  return self["getElementsByTagName"]($hs_fromUtf8(tagname));
                                                });
var webkit_dom_document_import_node;
webkit_dom_document_import_node = (function(self, importedNode,
                                            deep)
                                   {
                                     return self["importNode"](importedNode,
                                                               ($hs_intToNumber(deep) != 0));
                                   });
var webkit_dom_document_create_element_ns;
webkit_dom_document_create_element_ns = (function(self,
                                                  namespaceURI, qualifiedName)
                                         {
                                           return self["createElementNS"]($hs_fromUtf8(namespaceURI),
                                                                          $hs_fromUtf8(qualifiedName));
                                         });
var webkit_dom_document_create_attribute_ns;
webkit_dom_document_create_attribute_ns = (function(self,
                                                    namespaceURI, qualifiedName)
                                           {
                                             return self["createAttributeNS"]($hs_fromUtf8(namespaceURI),
                                                                              $hs_fromUtf8(qualifiedName));
                                           });
var webkit_dom_document_get_elements_by_tag_name_ns;
webkit_dom_document_get_elements_by_tag_name_ns = (function(self,
                                                            namespaceURI, localName)
                                                   {
                                                     return self["getElementsByTagNameNS"]($hs_fromUtf8(namespaceURI),
                                                                                           $hs_fromUtf8(localName));
                                                   });
var webkit_dom_document_get_element_by_id;
webkit_dom_document_get_element_by_id = (function(self, elementId)
                                         {
                                           return self["getElementById"]($hs_fromUtf8(elementId));
                                         });
var webkit_dom_document_adopt_node;
webkit_dom_document_adopt_node = (function(self, source)
                                  {
                                    return self["adoptNode"](source);
                                  });
var webkit_dom_document_create_event;
webkit_dom_document_create_event = (function(self, eventType)
                                    {
                                      return self["createEvent"]($hs_fromUtf8(eventType));
                                    });
var webkit_dom_document_create_range;
webkit_dom_document_create_range = (function(self)
                                    {
                                      return self["createRange"]();
                                    });
var webkit_dom_document_create_node_iterator;
webkit_dom_document_create_node_iterator = (function(self, root,
                                                     whatToShow, filter, expandEntityReferences)
                                            {
                                              return self["createNodeIterator"](root,
                                                                                $hs_intToNumber(whatToShow),
                                                                                filter,
                                                                                ($hs_intToNumber(expandEntityReferences)
                                                                                 !=
                                                                                 0));
                                            });
var webkit_dom_document_create_tree_walker;
webkit_dom_document_create_tree_walker = (function(self, root,
                                                   whatToShow, filter, expandEntityReferences)
                                          {
                                            return self["createTreeWalker"](root,
                                                                            $hs_intToNumber(whatToShow),
                                                                            filter,
                                                                            ($hs_intToNumber(expandEntityReferences)
                                                                             !=
                                                                             0));
                                          });
var webkit_dom_document_get_override_style;
webkit_dom_document_get_override_style = (function(self, element,
                                                   pseudoElement)
                                          {
                                            return self["getOverrideStyle"](element,
                                                                            $hs_fromUtf8(pseudoElement));
                                          });
var webkit_dom_document_create_expression;
webkit_dom_document_create_expression = (function(self, expression,
                                                  resolver)
                                         {
                                           return self["createExpression"]($hs_fromUtf8(expression),
                                                                           resolver);
                                         });
var webkit_dom_document_create_ns_resolver;
webkit_dom_document_create_ns_resolver = (function(self,
                                                   nodeResolver)
                                          {
                                            return self["createNSResolver"](nodeResolver);
                                          });
var webkit_dom_document_evaluate;
webkit_dom_document_evaluate = (function(self, expression,
                                         contextNode, resolver, type, inResult)
                                {
                                  return self["evaluate"]($hs_fromUtf8(expression), contextNode,
                                                          resolver, $hs_intToNumber(type),
                                                          inResult);
                                });
var webkit_dom_document_exec_command;
webkit_dom_document_exec_command = (function(self, command,
                                             userInterface, value)
                                    {
                                      return $hs_int((self["execCommand"]($hs_fromUtf8(command),
                                                                          ($hs_intToNumber(userInterface)
                                                                           !=
                                                                           0),
                                                                          $hs_fromUtf8(value)) ? 1 : 0));
                                    });
var webkit_dom_document_query_command_enabled;
webkit_dom_document_query_command_enabled = (function(self,
                                                      command)
                                             {
                                               return $hs_int((self["queryCommandEnabled"]($hs_fromUtf8(command)) ? 1 : 0));
                                             });
var webkit_dom_document_query_command_indeterm;
webkit_dom_document_query_command_indeterm = (function(self,
                                                       command)
                                              {
                                                return $hs_int((self["queryCommandIndeterm"]($hs_fromUtf8(command)) ? 1 : 0));
                                              });
var webkit_dom_document_query_command_state;
webkit_dom_document_query_command_state = (function(self, command)
                                           {
                                             return $hs_int((self["queryCommandState"]($hs_fromUtf8(command)) ? 1 : 0));
                                           });
var webkit_dom_document_query_command_supported;
webkit_dom_document_query_command_supported = (function(self,
                                                        command)
                                               {
                                                 return $hs_int((self["queryCommandSupported"]($hs_fromUtf8(command)) ? 1 : 0));
                                               });
var webkit_dom_document_query_command_value;
webkit_dom_document_query_command_value = (function(self, command)
                                           {
                                             return $hs_toUtf8(self["queryCommandValue"]($hs_fromUtf8(command)));
                                           });
var webkit_dom_document_get_elements_by_name;
webkit_dom_document_get_elements_by_name = (function(self,
                                                     elementName)
                                            {
                                              return self["getElementsByName"]($hs_fromUtf8(elementName));
                                            });
var webkit_dom_document_element_from_point;
webkit_dom_document_element_from_point = (function(self, x, y)
                                          {
                                            return self["elementFromPoint"]($hs_intToNumber(x),
                                                                            $hs_intToNumber(y));
                                          });
var webkit_dom_document_caret_range_from_point;
webkit_dom_document_caret_range_from_point = (function(self, x, y)
                                              {
                                                return self["caretRangeFromPoint"]($hs_intToNumber(x),
                                                                                   $hs_intToNumber(y));
                                              });
var webkit_dom_document_create_css_style_declaration;
webkit_dom_document_create_css_style_declaration = (function(self)
                                                    {
                                                      return self["createCSSStyleDeclaration"]();
                                                    });
var webkit_dom_document_get_elements_by_class_name;
webkit_dom_document_get_elements_by_class_name = (function(self,
                                                           tagname)
                                                  {
                                                    return self["getElementsByClassName"]($hs_fromUtf8(tagname));
                                                  });
var webkit_dom_document_query_selector;
webkit_dom_document_query_selector = (function(self, selectors)
                                      {
                                        return self["querySelector"]($hs_fromUtf8(selectors));
                                      });
var webkit_dom_document_query_selector_all;
webkit_dom_document_query_selector_all = (function(self, selectors)
                                          {
                                            return self["querySelectorAll"]($hs_fromUtf8(selectors));
                                          });
var webkit_dom_document_webkit_get_flow_by_name;
webkit_dom_document_webkit_get_flow_by_name = (function(self, name)
                                               {
                                                 return self["webkitGetFlowByName"]($hs_fromUtf8(name));
                                               });
var webkit_dom_document_get_doctype;
webkit_dom_document_get_doctype = (function(self)
                                   {
                                     return self["doctype"];
                                   });
var webkit_dom_document_get_implementation;
webkit_dom_document_get_implementation = (function(self)
                                          {
                                            return self["implementation"];
                                          });
var webkit_dom_document_get_document_element;
webkit_dom_document_get_document_element = (function(self)
                                            {
                                              return self["documentElement"];
                                            });
var webkit_dom_document_get_input_encoding;
webkit_dom_document_get_input_encoding = (function(self)
                                          {
                                            return $hs_toUtf8(self["inputEncoding"]);
                                          });
var webkit_dom_document_get_xml_encoding;
webkit_dom_document_get_xml_encoding = (function(self)
                                        {
                                          return $hs_toUtf8(self["xmlEncoding"]);
                                        });
var webkit_dom_document_set_xml_version;
webkit_dom_document_set_xml_version = (function(self, val)
                                       {
                                         self["xmlVersion"] = $hs_fromUtf8(val);
                                       });
var webkit_dom_document_get_xml_version;
webkit_dom_document_get_xml_version = (function(self)
                                       {
                                         return $hs_toUtf8(self["xmlVersion"]);
                                       });
var webkit_dom_document_set_xml_standalone;
webkit_dom_document_set_xml_standalone = (function(self, val)
                                          {
                                            self["xmlStandalone"] = ($hs_intToNumber(val) != 0);
                                          });
var webkit_dom_document_get_xml_standalone;
webkit_dom_document_get_xml_standalone = (function(self)
                                          {
                                            return $hs_int((self["xmlStandalone"] ? 1 : 0));
                                          });
var webkit_dom_document_set_document_uri;
webkit_dom_document_set_document_uri = (function(self, val)
                                        {
                                          self["documentURI"] = $hs_fromUtf8(val);
                                        });
var webkit_dom_document_get_document_uri;
webkit_dom_document_get_document_uri = (function(self)
                                        {
                                          return $hs_toUtf8(self["documentURI"]);
                                        });
var webkit_dom_document_get_default_view;
webkit_dom_document_get_default_view = (function(self)
                                        {
                                          return self["defaultView"];
                                        });
var webkit_dom_document_get_style_sheets;
webkit_dom_document_get_style_sheets = (function(self)
                                        {
                                          return self["styleSheets"];
                                        });
var webkit_dom_document_set_title;
webkit_dom_document_set_title = (function(self, val)
                                 {
                                   self["title"] = $hs_fromUtf8(val);
                                 });
var webkit_dom_document_get_title;
webkit_dom_document_get_title = (function(self)
                                 {
                                   return $hs_toUtf8(self["title"]);
                                 });
var webkit_dom_document_get_referrer;
webkit_dom_document_get_referrer = (function(self)
                                    {
                                      return $hs_toUtf8(self["referrer"]);
                                    });
var webkit_dom_document_get_domain;
webkit_dom_document_get_domain = (function(self)
                                  {
                                    return $hs_toUtf8(self["domain"]);
                                  });
var webkit_dom_document_set_cookie;
webkit_dom_document_set_cookie = (function(self, val)
                                  {
                                    self["cookie"] = $hs_fromUtf8(val);
                                  });
var webkit_dom_document_get_cookie;
webkit_dom_document_get_cookie = (function(self)
                                  {
                                    return $hs_toUtf8(self["cookie"]);
                                  });
var webkit_dom_document_set_body;
webkit_dom_document_set_body = (function(self, val)
                                {
                                  self["body"] = val;
                                });
var webkit_dom_document_get_body;
webkit_dom_document_get_body = (function(self)
                                {
                                  return self["body"];
                                });
var webkit_dom_document_get_head;
webkit_dom_document_get_head = (function(self)
                                {
                                  return self["head"];
                                });
var webkit_dom_document_get_images;
webkit_dom_document_get_images = (function(self)
                                  {
                                    return self["images"];
                                  });
var webkit_dom_document_get_applets;
webkit_dom_document_get_applets = (function(self)
                                   {
                                     return self["applets"];
                                   });
var webkit_dom_document_get_links;
webkit_dom_document_get_links = (function(self)
                                 {
                                   return self["links"];
                                 });
var webkit_dom_document_get_forms;
webkit_dom_document_get_forms = (function(self)
                                 {
                                   return self["forms"];
                                 });
var webkit_dom_document_get_anchors;
webkit_dom_document_get_anchors = (function(self)
                                   {
                                     return self["anchors"];
                                   });
var webkit_dom_document_get_last_modified;
webkit_dom_document_get_last_modified = (function(self)
                                         {
                                           return $hs_toUtf8(self["lastModified"]);
                                         });
var webkit_dom_document_set_charset;
webkit_dom_document_set_charset = (function(self, val)
                                   {
                                     self["charset"] = $hs_fromUtf8(val);
                                   });
var webkit_dom_document_get_charset;
webkit_dom_document_get_charset = (function(self)
                                   {
                                     return $hs_toUtf8(self["charset"]);
                                   });
var webkit_dom_document_get_default_charset;
webkit_dom_document_get_default_charset = (function(self)
                                           {
                                             return $hs_toUtf8(self["defaultCharset"]);
                                           });
var webkit_dom_document_get_ready_state;
webkit_dom_document_get_ready_state = (function(self)
                                       {
                                         return $hs_toUtf8(self["readyState"]);
                                       });
var webkit_dom_document_get_character_set;
webkit_dom_document_get_character_set = (function(self)
                                         {
                                           return $hs_toUtf8(self["characterSet"]);
                                         });
var webkit_dom_document_get_preferred_stylesheet_set;
webkit_dom_document_get_preferred_stylesheet_set = (function(self)
                                                    {
                                                      return $hs_toUtf8(self["preferredStylesheetSet"]);
                                                    });
var webkit_dom_document_set_selected_stylesheet_set;
webkit_dom_document_set_selected_stylesheet_set = (function(self,
                                                            val)
                                                   {
                                                     self["selectedStylesheetSet"] = $hs_fromUtf8(val);
                                                   });
var webkit_dom_document_get_selected_stylesheet_set;
webkit_dom_document_get_selected_stylesheet_set = (function(self)
                                                   {
                                                     return $hs_toUtf8(self["selectedStylesheetSet"]);
                                                   });
var webkit_dom_document_get_compat_mode;
webkit_dom_document_get_compat_mode = (function(self)
                                       {
                                         return $hs_toUtf8(self["compatMode"]);
                                       });
var webkit_dom_document_set_onabort;
webkit_dom_document_set_onabort = (function(self, val)
                                   {
                                     self["onabort"] = val;
                                   });
var webkit_dom_document_get_onabort;
webkit_dom_document_get_onabort = (function(self)
                                   {
                                     return self["onabort"];
                                   });
var webkit_dom_document_set_onblur;
webkit_dom_document_set_onblur = (function(self, val)
                                  {
                                    self["onblur"] = val;
                                  });
var webkit_dom_document_get_onblur;
webkit_dom_document_get_onblur = (function(self)
                                  {
                                    return self["onblur"];
                                  });
var webkit_dom_document_set_onchange;
webkit_dom_document_set_onchange = (function(self, val)
                                    {
                                      self["onchange"] = val;
                                    });
var webkit_dom_document_get_onchange;
webkit_dom_document_get_onchange = (function(self)
                                    {
                                      return self["onchange"];
                                    });
var webkit_dom_document_set_onclick;
webkit_dom_document_set_onclick = (function(self, val)
                                   {
                                     self["onclick"] = val;
                                   });
var webkit_dom_document_get_onclick;
webkit_dom_document_get_onclick = (function(self)
                                   {
                                     return self["onclick"];
                                   });
var webkit_dom_document_set_oncontextmenu;
webkit_dom_document_set_oncontextmenu = (function(self, val)
                                         {
                                           self["oncontextmenu"] = val;
                                         });
var webkit_dom_document_get_oncontextmenu;
webkit_dom_document_get_oncontextmenu = (function(self)
                                         {
                                           return self["oncontextmenu"];
                                         });
var webkit_dom_document_set_ondblclick;
webkit_dom_document_set_ondblclick = (function(self, val)
                                      {
                                        self["ondblclick"] = val;
                                      });
var webkit_dom_document_get_ondblclick;
webkit_dom_document_get_ondblclick = (function(self)
                                      {
                                        return self["ondblclick"];
                                      });
var webkit_dom_document_set_ondrag;
webkit_dom_document_set_ondrag = (function(self, val)
                                  {
                                    self["ondrag"] = val;
                                  });
var webkit_dom_document_get_ondrag;
webkit_dom_document_get_ondrag = (function(self)
                                  {
                                    return self["ondrag"];
                                  });
var webkit_dom_document_set_ondragend;
webkit_dom_document_set_ondragend = (function(self, val)
                                     {
                                       self["ondragend"] = val;
                                     });
var webkit_dom_document_get_ondragend;
webkit_dom_document_get_ondragend = (function(self)
                                     {
                                       return self["ondragend"];
                                     });
var webkit_dom_document_set_ondragenter;
webkit_dom_document_set_ondragenter = (function(self, val)
                                       {
                                         self["ondragenter"] = val;
                                       });
var webkit_dom_document_get_ondragenter;
webkit_dom_document_get_ondragenter = (function(self)
                                       {
                                         return self["ondragenter"];
                                       });
var webkit_dom_document_set_ondragleave;
webkit_dom_document_set_ondragleave = (function(self, val)
                                       {
                                         self["ondragleave"] = val;
                                       });
var webkit_dom_document_get_ondragleave;
webkit_dom_document_get_ondragleave = (function(self)
                                       {
                                         return self["ondragleave"];
                                       });
var webkit_dom_document_set_ondragover;
webkit_dom_document_set_ondragover = (function(self, val)
                                      {
                                        self["ondragover"] = val;
                                      });
var webkit_dom_document_get_ondragover;
webkit_dom_document_get_ondragover = (function(self)
                                      {
                                        return self["ondragover"];
                                      });
var webkit_dom_document_set_ondragstart;
webkit_dom_document_set_ondragstart = (function(self, val)
                                       {
                                         self["ondragstart"] = val;
                                       });
var webkit_dom_document_get_ondragstart;
webkit_dom_document_get_ondragstart = (function(self)
                                       {
                                         return self["ondragstart"];
                                       });
var webkit_dom_document_set_ondrop;
webkit_dom_document_set_ondrop = (function(self, val)
                                  {
                                    self["ondrop"] = val;
                                  });
var webkit_dom_document_get_ondrop;
webkit_dom_document_get_ondrop = (function(self)
                                  {
                                    return self["ondrop"];
                                  });
var webkit_dom_document_set_onerror;
webkit_dom_document_set_onerror = (function(self, val)
                                   {
                                     self["onerror"] = val;
                                   });
var webkit_dom_document_get_onerror;
webkit_dom_document_get_onerror = (function(self)
                                   {
                                     return self["onerror"];
                                   });
var webkit_dom_document_set_onfocus;
webkit_dom_document_set_onfocus = (function(self, val)
                                   {
                                     self["onfocus"] = val;
                                   });
var webkit_dom_document_get_onfocus;
webkit_dom_document_get_onfocus = (function(self)
                                   {
                                     return self["onfocus"];
                                   });
var webkit_dom_document_set_oninput;
webkit_dom_document_set_oninput = (function(self, val)
                                   {
                                     self["oninput"] = val;
                                   });
var webkit_dom_document_get_oninput;
webkit_dom_document_get_oninput = (function(self)
                                   {
                                     return self["oninput"];
                                   });
var webkit_dom_document_set_oninvalid;
webkit_dom_document_set_oninvalid = (function(self, val)
                                     {
                                       self["oninvalid"] = val;
                                     });
var webkit_dom_document_get_oninvalid;
webkit_dom_document_get_oninvalid = (function(self)
                                     {
                                       return self["oninvalid"];
                                     });
var webkit_dom_document_set_onkeydown;
webkit_dom_document_set_onkeydown = (function(self, val)
                                     {
                                       self["onkeydown"] = val;
                                     });
var webkit_dom_document_get_onkeydown;
webkit_dom_document_get_onkeydown = (function(self)
                                     {
                                       return self["onkeydown"];
                                     });
var webkit_dom_document_set_onkeypress;
webkit_dom_document_set_onkeypress = (function(self, val)
                                      {
                                        self["onkeypress"] = val;
                                      });
var webkit_dom_document_get_onkeypress;
webkit_dom_document_get_onkeypress = (function(self)
                                      {
                                        return self["onkeypress"];
                                      });
var webkit_dom_document_set_onkeyup;
webkit_dom_document_set_onkeyup = (function(self, val)
                                   {
                                     self["onkeyup"] = val;
                                   });
var webkit_dom_document_get_onkeyup;
webkit_dom_document_get_onkeyup = (function(self)
                                   {
                                     return self["onkeyup"];
                                   });
var webkit_dom_document_set_onload;
webkit_dom_document_set_onload = (function(self, val)
                                  {
                                    self["onload"] = val;
                                  });
var webkit_dom_document_get_onload;
webkit_dom_document_get_onload = (function(self)
                                  {
                                    return self["onload"];
                                  });
var webkit_dom_document_set_onmousedown;
webkit_dom_document_set_onmousedown = (function(self, val)
                                       {
                                         self["onmousedown"] = val;
                                       });
var webkit_dom_document_get_onmousedown;
webkit_dom_document_get_onmousedown = (function(self)
                                       {
                                         return self["onmousedown"];
                                       });
var webkit_dom_document_set_onmousemove;
webkit_dom_document_set_onmousemove = (function(self, val)
                                       {
                                         self["onmousemove"] = val;
                                       });
var webkit_dom_document_get_onmousemove;
webkit_dom_document_get_onmousemove = (function(self)
                                       {
                                         return self["onmousemove"];
                                       });
var webkit_dom_document_set_onmouseout;
webkit_dom_document_set_onmouseout = (function(self, val)
                                      {
                                        self["onmouseout"] = val;
                                      });
var webkit_dom_document_get_onmouseout;
webkit_dom_document_get_onmouseout = (function(self)
                                      {
                                        return self["onmouseout"];
                                      });
var webkit_dom_document_set_onmouseover;
webkit_dom_document_set_onmouseover = (function(self, val)
                                       {
                                         self["onmouseover"] = val;
                                       });
var webkit_dom_document_get_onmouseover;
webkit_dom_document_get_onmouseover = (function(self)
                                       {
                                         return self["onmouseover"];
                                       });
var webkit_dom_document_set_onmouseup;
webkit_dom_document_set_onmouseup = (function(self, val)
                                     {
                                       self["onmouseup"] = val;
                                     });
var webkit_dom_document_get_onmouseup;
webkit_dom_document_get_onmouseup = (function(self)
                                     {
                                       return self["onmouseup"];
                                     });
var webkit_dom_document_set_onmousewheel;
webkit_dom_document_set_onmousewheel = (function(self, val)
                                        {
                                          self["onmousewheel"] = val;
                                        });
var webkit_dom_document_get_onmousewheel;
webkit_dom_document_get_onmousewheel = (function(self)
                                        {
                                          return self["onmousewheel"];
                                        });
var webkit_dom_document_set_onreadystatechange;
webkit_dom_document_set_onreadystatechange = (function(self, val)
                                              {
                                                self["onreadystatechange"] = val;
                                              });
var webkit_dom_document_get_onreadystatechange;
webkit_dom_document_get_onreadystatechange = (function(self)
                                              {
                                                return self["onreadystatechange"];
                                              });
var webkit_dom_document_set_onscroll;
webkit_dom_document_set_onscroll = (function(self, val)
                                    {
                                      self["onscroll"] = val;
                                    });
var webkit_dom_document_get_onscroll;
webkit_dom_document_get_onscroll = (function(self)
                                    {
                                      return self["onscroll"];
                                    });
var webkit_dom_document_set_onselect;
webkit_dom_document_set_onselect = (function(self, val)
                                    {
                                      self["onselect"] = val;
                                    });
var webkit_dom_document_get_onselect;
webkit_dom_document_get_onselect = (function(self)
                                    {
                                      return self["onselect"];
                                    });
var webkit_dom_document_set_onsubmit;
webkit_dom_document_set_onsubmit = (function(self, val)
                                    {
                                      self["onsubmit"] = val;
                                    });
var webkit_dom_document_get_onsubmit;
webkit_dom_document_get_onsubmit = (function(self)
                                    {
                                      return self["onsubmit"];
                                    });
var webkit_dom_document_set_onbeforecut;
webkit_dom_document_set_onbeforecut = (function(self, val)
                                       {
                                         self["onbeforecut"] = val;
                                       });
var webkit_dom_document_get_onbeforecut;
webkit_dom_document_get_onbeforecut = (function(self)
                                       {
                                         return self["onbeforecut"];
                                       });
var webkit_dom_document_set_oncut;
webkit_dom_document_set_oncut = (function(self, val)
                                 {
                                   self["oncut"] = val;
                                 });
var webkit_dom_document_get_oncut;
webkit_dom_document_get_oncut = (function(self)
                                 {
                                   return self["oncut"];
                                 });
var webkit_dom_document_set_onbeforecopy;
webkit_dom_document_set_onbeforecopy = (function(self, val)
                                        {
                                          self["onbeforecopy"] = val;
                                        });
var webkit_dom_document_get_onbeforecopy;
webkit_dom_document_get_onbeforecopy = (function(self)
                                        {
                                          return self["onbeforecopy"];
                                        });
var webkit_dom_document_set_oncopy;
webkit_dom_document_set_oncopy = (function(self, val)
                                  {
                                    self["oncopy"] = val;
                                  });
var webkit_dom_document_get_oncopy;
webkit_dom_document_get_oncopy = (function(self)
                                  {
                                    return self["oncopy"];
                                  });
var webkit_dom_document_set_onbeforepaste;
webkit_dom_document_set_onbeforepaste = (function(self, val)
                                         {
                                           self["onbeforepaste"] = val;
                                         });
var webkit_dom_document_get_onbeforepaste;
webkit_dom_document_get_onbeforepaste = (function(self)
                                         {
                                           return self["onbeforepaste"];
                                         });
var webkit_dom_document_set_onpaste;
webkit_dom_document_set_onpaste = (function(self, val)
                                   {
                                     self["onpaste"] = val;
                                   });
var webkit_dom_document_get_onpaste;
webkit_dom_document_get_onpaste = (function(self)
                                   {
                                     return self["onpaste"];
                                   });
var webkit_dom_document_set_onreset;
webkit_dom_document_set_onreset = (function(self, val)
                                   {
                                     self["onreset"] = val;
                                   });
var webkit_dom_document_get_onreset;
webkit_dom_document_get_onreset = (function(self)
                                   {
                                     return self["onreset"];
                                   });
var webkit_dom_document_set_onsearch;
webkit_dom_document_set_onsearch = (function(self, val)
                                    {
                                      self["onsearch"] = val;
                                    });
var webkit_dom_document_get_onsearch;
webkit_dom_document_get_onsearch = (function(self)
                                    {
                                      return self["onsearch"];
                                    });
var webkit_dom_document_set_onselectstart;
webkit_dom_document_set_onselectstart = (function(self, val)
                                         {
                                           self["onselectstart"] = val;
                                         });
var webkit_dom_document_get_onselectstart;
webkit_dom_document_get_onselectstart = (function(self)
                                         {
                                           return self["onselectstart"];
                                         });
var webkit_dom_document_set_onselectionchange;
webkit_dom_document_set_onselectionchange = (function(self, val)
                                             {
                                               self["onselectionchange"] = val;
                                             });
var webkit_dom_document_get_onselectionchange;
webkit_dom_document_get_onselectionchange = (function(self)
                                             {
                                               return self["onselectionchange"];
                                             });
var webkit_dom_document_set_ontouchstart;
webkit_dom_document_set_ontouchstart = (function(self, val)
                                        {
                                          self["ontouchstart"] = val;
                                        });
var webkit_dom_document_get_ontouchstart;
webkit_dom_document_get_ontouchstart = (function(self)
                                        {
                                          return self["ontouchstart"];
                                        });
var webkit_dom_document_set_ontouchmove;
webkit_dom_document_set_ontouchmove = (function(self, val)
                                       {
                                         self["ontouchmove"] = val;
                                       });
var webkit_dom_document_get_ontouchmove;
webkit_dom_document_get_ontouchmove = (function(self)
                                       {
                                         return self["ontouchmove"];
                                       });
var webkit_dom_document_set_ontouchend;
webkit_dom_document_set_ontouchend = (function(self, val)
                                      {
                                        self["ontouchend"] = val;
                                      });
var webkit_dom_document_get_ontouchend;
webkit_dom_document_get_ontouchend = (function(self)
                                      {
                                        return self["ontouchend"];
                                      });
var webkit_dom_document_set_ontouchcancel;
webkit_dom_document_set_ontouchcancel = (function(self, val)
                                         {
                                           self["ontouchcancel"] = val;
                                         });
var webkit_dom_document_get_ontouchcancel;
webkit_dom_document_get_ontouchcancel = (function(self)
                                         {
                                           return self["ontouchcancel"];
                                         });
var webkit_dom_document_set_onwebkitfullscreenchange;
webkit_dom_document_set_onwebkitfullscreenchange = (function(self,
                                                             val)
                                                    {
                                                      self["onwebkitfullscreenchange"] = val;
                                                    });
var webkit_dom_document_get_onwebkitfullscreenchange;
webkit_dom_document_get_onwebkitfullscreenchange = (function(self)
                                                    {
                                                      return self["onwebkitfullscreenchange"];
                                                    });
var webkit_dom_document_set_onwebkitfullscreenerror;
webkit_dom_document_set_onwebkitfullscreenerror = (function(self,
                                                            val)
                                                   {
                                                     self["onwebkitfullscreenerror"] = val;
                                                   });
var webkit_dom_document_get_onwebkitfullscreenerror;
webkit_dom_document_get_onwebkitfullscreenerror = (function(self)
                                                   {
                                                     return self["onwebkitfullscreenerror"];
                                                   });
var webkit_dom_document_get_webkit_visibility_state;
webkit_dom_document_get_webkit_visibility_state = (function(self)
                                                   {
                                                     return $hs_toUtf8(self["webkitVisibilityState"]);
                                                   });
var webkit_dom_document_get_webkit_hidden;
webkit_dom_document_get_webkit_hidden = (function(self)
                                         {
                                           return $hs_int((self["webkitHidden"] ? 1 : 0));
                                         });
// Graphics.UI.Gtk.WebKit.DOM.Css
webkit_dom_css_value_get_type = (function()
                                 {
                                   return CSSValue;
                                 });
var webkit_dom_css_value_set_css_text;
webkit_dom_css_value_set_css_text = (function(self, val)
                                     {
                                       self["cssText"] = $hs_fromUtf8(val);
                                     });
var webkit_dom_css_value_get_css_text;
webkit_dom_css_value_get_css_text = (function(self)
                                     {
                                       return $hs_toUtf8(self["cssText"]);
                                     });
var webkit_dom_css_value_get_css_value_type;
webkit_dom_css_value_get_css_value_type = (function(self)
                                           {
                                             return $hs_int(self["cssValueType"]);
                                           });
// Graphics.UI.Gtk.WebKit.DOM.Css
webkit_dom_css_style_sheet_get_type = (function()
                                       {
                                         return CSSStyleSheet;
                                       });
var webkit_dom_css_style_sheet_insert_rule;
webkit_dom_css_style_sheet_insert_rule = (function(self, rule,
                                                   index)
                                          {
                                            return $hs_int(self["insertRule"]($hs_fromUtf8(rule),
                                                                              $hs_intToNumber(index)));
                                          });
var webkit_dom_css_style_sheet_delete_rule;
webkit_dom_css_style_sheet_delete_rule = (function(self, index)
                                          {
                                            return self["deleteRule"]($hs_intToNumber(index));
                                          });
var webkit_dom_css_style_sheet_add_rule;
webkit_dom_css_style_sheet_add_rule = (function(self, selector,
                                                style, index)
                                       {
                                         return $hs_int(self["addRule"]($hs_fromUtf8(selector),
                                                                        $hs_fromUtf8(style),
                                                                        $hs_intToNumber(index)));
                                       });
var webkit_dom_css_style_sheet_remove_rule;
webkit_dom_css_style_sheet_remove_rule = (function(self, index)
                                          {
                                            return self["removeRule"]($hs_intToNumber(index));
                                          });
var webkit_dom_css_style_sheet_get_owner_rule;
webkit_dom_css_style_sheet_get_owner_rule = (function(self)
                                             {
                                               return self["ownerRule"];
                                             });
var webkit_dom_css_style_sheet_get_css_rules;
webkit_dom_css_style_sheet_get_css_rules = (function(self)
                                            {
                                              return self["cssRules"];
                                            });
var webkit_dom_css_style_sheet_get_rules;
webkit_dom_css_style_sheet_get_rules = (function(self)
                                        {
                                          return self["rules"];
                                        });
// Graphics.UI.Gtk.WebKit.DOM.Css
webkit_dom_css_style_declaration_get_type = (function()
                                             {
                                               return CSSStyleDeclaration;
                                             });
var webkit_dom_css_style_declaration_get_property_value;
webkit_dom_css_style_declaration_get_property_value = (function(self,
                                                                propertyName)
                                                       {
                                                         return $hs_toUtf8(self["getPropertyValue"]($hs_fromUtf8(propertyName)));
                                                       });
var webkit_dom_css_style_declaration_get_property_css_value;
webkit_dom_css_style_declaration_get_property_css_value = (function(self,
                                                                    propertyName)
                                                           {
                                                             return self["getPropertyCSSValue"]($hs_fromUtf8(propertyName));
                                                           });
var webkit_dom_css_style_declaration_remove_property;
webkit_dom_css_style_declaration_remove_property = (function(self,
                                                             propertyName)
                                                    {
                                                      return $hs_toUtf8(self["removeProperty"]($hs_fromUtf8(propertyName)));
                                                    });
var webkit_dom_css_style_declaration_get_property_priority;
webkit_dom_css_style_declaration_get_property_priority = (function(self,
                                                                   propertyName)
                                                          {
                                                            return $hs_toUtf8(self["getPropertyPriority"]($hs_fromUtf8(propertyName)));
                                                          });
var webkit_dom_css_style_declaration_set_property;
webkit_dom_css_style_declaration_set_property = (function(self,
                                                          propertyName, value, priority)
                                                 {
                                                   return self["setProperty"]($hs_fromUtf8(propertyName),
                                                                              $hs_fromUtf8(value),
                                                                              $hs_fromUtf8(priority));
                                                 });
var webkit_dom_css_style_declaration_item;
webkit_dom_css_style_declaration_item = (function(self, index)
                                         {
                                           return $hs_toUtf8(self["item"]($hs_intToNumber(index)));
                                         });
var webkit_dom_css_style_declaration_get_property_shorthand;
webkit_dom_css_style_declaration_get_property_shorthand = (function(self,
                                                                    propertyName)
                                                           {
                                                             return $hs_toUtf8(self["getPropertyShorthand"]($hs_fromUtf8(propertyName)));
                                                           });
var webkit_dom_css_style_declaration_is_property_implicit;
webkit_dom_css_style_declaration_is_property_implicit = (function(self,
                                                                  propertyName)
                                                         {
                                                           return $hs_int((self["isPropertyImplicit"]($hs_fromUtf8(propertyName)) ? 1 : 0));
                                                         });
var webkit_dom_css_style_declaration_set_css_text;
webkit_dom_css_style_declaration_set_css_text = (function(self,
                                                          val)
                                                 {
                                                   self["cssText"] = $hs_fromUtf8(val);
                                                 });
var webkit_dom_css_style_declaration_get_css_text;
webkit_dom_css_style_declaration_get_css_text = (function(self)
                                                 {
                                                   return $hs_toUtf8(self["cssText"]);
                                                 });
var webkit_dom_css_style_declaration_get_length;
webkit_dom_css_style_declaration_get_length = (function(self)
                                               {
                                                 return $hs_int(self["length"]);
                                               });
var webkit_dom_css_style_declaration_get_parent_rule;
webkit_dom_css_style_declaration_get_parent_rule = (function(self)
                                                    {
                                                      return self["parentRule"];
                                                    });
// Graphics.UI.Gtk.WebKit.DOM.Css
webkit_dom_css_rule_list_get_type = (function()
                                     {
                                       return CSSRuleList;
                                     });
var webkit_dom_css_rule_list_item;
webkit_dom_css_rule_list_item = (function(self, index)
                                 {
                                   return self["item"]($hs_intToNumber(index));
                                 });
var webkit_dom_css_rule_list_get_length;
webkit_dom_css_rule_list_get_length = (function(self)
                                       {
                                         return $hs_int(self["length"]);
                                       });
// Graphics.UI.Gtk.WebKit.DOM.Css
webkit_dom_css_rule_get_type = (function()
                                {
                                  return CSSRule;
                                });
var webkit_dom_css_rule_set_css_text;
webkit_dom_css_rule_set_css_text = (function(self, val)
                                    {
                                      self["cssText"] = $hs_fromUtf8(val);
                                    });
var webkit_dom_css_rule_get_css_text;
webkit_dom_css_rule_get_css_text = (function(self)
                                    {
                                      return $hs_toUtf8(self["cssText"]);
                                    });
var webkit_dom_css_rule_get_parent_style_sheet;
webkit_dom_css_rule_get_parent_style_sheet = (function(self)
                                              {
                                                return self["parentStyleSheet"];
                                              });
var webkit_dom_css_rule_get_parent_rule;
webkit_dom_css_rule_get_parent_rule = (function(self)
                                       {
                                         return self["parentRule"];
                                       });
// Graphics.UI.Gtk.WebKit.DOM.Window
webkit_dom_console_get_type = (function()
                               {
                                 return Console;
                               });
var webkit_dom_console_time;
webkit_dom_console_time = (function(self, title)
                           {
                             return self["time"]($hs_fromUtf8(title));
                           });
var webkit_dom_console_group_end;
webkit_dom_console_group_end = (function(self)
                                {
                                  return self["groupEnd"]();
                                });
var webkit_dom_console_get_memory;
webkit_dom_console_get_memory = (function(self)
                                 {
                                   return self["memory"];
                                 });
// Graphics.UI.Gtk.WebKit.DOM.Core
webkit_dom_comment_get_type = (function()
                               {
                                 return Comment;
                               });
// Graphics.UI.Gtk.WebKit.DOM.Core
webkit_dom_character_data_get_type = (function()
                                      {
                                        return CharacterData;
                                      });
var webkit_dom_character_data_substring_data;
webkit_dom_character_data_substring_data = (function(self, offset,
                                                     length)
                                            {
                                              return $hs_toUtf8(self["substringData"]($hs_intToNumber(offset),
                                                                                      $hs_intToNumber(length)));
                                            });
var webkit_dom_character_data_append_data;
webkit_dom_character_data_append_data = (function(self, data)
                                         {
                                           return self["appendData"]($hs_fromUtf8(data));
                                         });
var webkit_dom_character_data_insert_data;
webkit_dom_character_data_insert_data = (function(self, offset,
                                                  data)
                                         {
                                           return self["insertData"]($hs_intToNumber(offset),
                                                                     $hs_fromUtf8(data));
                                         });
var webkit_dom_character_data_delete_data;
webkit_dom_character_data_delete_data = (function(self, offset,
                                                  length)
                                         {
                                           return self["deleteData"]($hs_intToNumber(offset),
                                                                     $hs_intToNumber(length));
                                         });
var webkit_dom_character_data_replace_data;
webkit_dom_character_data_replace_data = (function(self, offset,
                                                   length, data)
                                          {
                                            return self["replaceData"]($hs_intToNumber(offset),
                                                                       $hs_intToNumber(length),
                                                                       $hs_fromUtf8(data));
                                          });
var webkit_dom_character_data_set_data;
webkit_dom_character_data_set_data = (function(self, val)
                                      {
                                        self["data"] = $hs_fromUtf8(val);
                                      });
var webkit_dom_character_data_get_data;
webkit_dom_character_data_get_data = (function(self)
                                      {
                                        return $hs_toUtf8(self["data"]);
                                      });
var webkit_dom_character_data_get_length;
webkit_dom_character_data_get_length = (function(self)
                                        {
                                          return $hs_int(self["length"]);
                                        });
// Graphics.UI.Gtk.WebKit.DOM.Core
webkit_dom_cdata_section_get_type = (function()
                                     {
                                       return CDATASection;
                                     });
// Graphics.UI.Gtk.WebKit.DOM.Html
webkit_dom_blob_get_type = (function()
                            {
                              return Blob;
                            });
var webkit_dom_blob_get_size;
webkit_dom_blob_get_size = (function(self)
                            {
                              return $hs_int(self["size"]);
                            });
// Graphics.UI.Gtk.WebKit.DOM.Window
webkit_dom_bar_info_get_type = (function()
                                {
                                  return BarInfo;
                                });
var webkit_dom_bar_info_get_visible;
webkit_dom_bar_info_get_visible = (function(self)
                                   {
                                     return $hs_int((self["visible"] ? 1 : 0));
                                   });
// Graphics.UI.Gtk.WebKit.DOM.Core
webkit_dom_attr_get_type = (function()
                            {
                              return Attr;
                            });
var webkit_dom_attr_get_name;
webkit_dom_attr_get_name = (function(self)
                            {
                              return $hs_toUtf8(self["name"]);
                            });
var webkit_dom_attr_get_specified;
webkit_dom_attr_get_specified = (function(self)
                                 {
                                   return $hs_int((self["specified"] ? 1 : 0));
                                 });
var webkit_dom_attr_set_value;
webkit_dom_attr_set_value = (function(self, val)
                             {
                               self["value"] = $hs_fromUtf8(val);
                             });
var webkit_dom_attr_get_value;
webkit_dom_attr_get_value = (function(self)
                             {
                               return $hs_toUtf8(self["value"]);
                             });
var webkit_dom_attr_get_owner_element;
webkit_dom_attr_get_owner_element = (function(self)
                                     {
                                       return self["ownerElement"];
                                     });
var webkit_dom_attr_get_is_id;
webkit_dom_attr_get_is_id = (function(self)
                             {
                               return $hs_int((self["isId"] ? 1 : 0));
                             });

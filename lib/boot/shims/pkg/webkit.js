function h$webkit_web_view_get_dom_document(w, w_2) {
  h$ret1 = 0
  return w.document;
};
function h$webkit_web_view_get_main_frame(w, w_2) {
  h$ret1 = 0;
  return w;
};
function h$webkit_web_frame_get_global_context(f, f_2) {
  h$ret1 = 0;
  return f;
};
function h$webkit_dom_event_target_add_event_listener_closure(obj, obj_2, eventName, eventName_2, f, f_2, bubble) {
  obj.addEventListener(h$decodeUtf8z(eventName, eventName_2), function(e) {
    h$run(h$c3(h$ap2_e, f.arr[0], h$mkPtr(obj, obj_2), h$mkPtr(e,0)));
  });
  return 1;
};


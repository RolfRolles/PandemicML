  // Creating the tform:
  HWND hwnd = NULL;
  TForm *form = create_tform("Sample custom view", &hwnd);
  if ( hwnd == NULL )
  {
    warning("Could not create custom view window\n"
            "perhaps it is open?\n"
            "Switching to it.");
    form = find_tform("Sample custom view");
    if ( form != NULL )
      switchto_tform(form, true);
    return;
  }

  // Inserting the lines, in order
  for ( int i=0; i < qnumber(sample_text); i++ )
  {
    si->sv.push_back(simpleline_t("")); // add empty line
    si->sv.push_back(simpleline_t(sample_text[i].text));
    si->sv.back().bgcolor = sample_text[i].color;
  }

  
  // Creating the custom viewer
  simpleline_place_t s1;
  simpleline_place_t s2(si->sv.size()-1);
  // create a custom viewer
  si->cv = create_custom_viewer("", (TWinControl *)form, &s1, &s2, &s1, 0, &si->sv);
  // set the handlers so we can communicate with it
  set_custom_viewer_handlers(si->cv, ct_keyboard, ct_popup, NULL, ct_curpos, NULL, si);
  // also set the ui event callback
  hook_to_notification_point(HT_UI, ui_callback, si);
  // finally display the form on the screen
  open_tform(form, FORM_TAB|FORM_MENU|FORM_RESTORE);

We can do hints inside of the UI callback:

    case ui_get_custom_viewer_hint:
      {
        TCustomControl *viewer = va_arg(va, TCustomControl *);
        place_t *place         = va_arg(va, place_t *);
        int *important_lines   = va_arg(va, int *);
        qstring &hint          = *va_arg(va, qstring *);
        if ( si->cv == viewer ) // our viewer
        {
          if ( place == NULL )
            return 0;
          simpleline_place_t *spl = (simpleline_place_t *)place;
          hint.sprnt("Hint for line %d", spl->n);
          *important_lines = 1;
          return 1;
        }
        break;
      }

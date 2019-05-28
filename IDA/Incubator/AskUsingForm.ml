OK, so this seems amenable to some high-level representation... sort of...

The first question that comes to mind is, how do values come back?
I could have something like
type struct = 
{
  mutable blah: int32;
  mutable blee: string;
  mutable bloo: char;
  mutable blor: bool;
  mutable blof: float;
}

type input_type = 

let enc_char_of_input_type = function
| AsciiString     -> 'A' (* char *, MAXSTR *)
| Segment         -> 'S' (* sel_t * *)
| HexPrefix       -> 'N' (* uval_t * *)
| SignedHexPrefix -> 'n' (* sval_t * *)
| Base            -> 'L' (* uint64 * *)
| SignedBase      -> 'l' (* int64 * *) 
| Hex             -> 'M' (* uval_t *)  
| Decimal         -> 'D' (* sval_t * *)
| Octal           -> 'O' (* sval_t * *)
| Binary          -> 'Y' (* sval_t * *)
| Char            -> 'H' (* sval_t * *)
| Address         -> '$' (* ea_t * *)
| Identifier      -> 'I' (* char *, MAXNAMELEN *)
| Button          -> 'B' (* formcb_t *)
| Hyperlink       -> 'k' (* formcb_t *)
| ColorButton     -> 'K' (* bgcolor_t * *)


  A - ascii string                                char* at least MAXSTR size
  S - segment                                     sel_t*
  N - hex number, C notation                      uval_t*
  n - signed hex number, C notation               sval_t*
  L - default base (usually hex) number,          uint64*
      C notation
  l - default base (usually hex) number, signed,  int64*
      C notation
  M - hex number, no "0x" prefix                  uval_t*
  D - decimal number                              sval_t*
  O - octal number, C notation                    sval_t*
  Y - binary number, "0b" prefix                  sval_t*
  H - char value, C notation                      sval_t*
  $ - address                                     ea_t*
  I - ident                                       char* at least MAXNAMELEN size
  B - button                                      formcb_t
  k - txt: button (same as B)/gui: hyperlink      formcb_t
  K - color button                                bgcolor_t*
  F - path to folder                              char* at least QMAXPATH size
  f - path to file                                char* at least QMAXPATH size
  T - type declaration                            char* at least MAXSTR size
  E - chooser                                     chooser_info_t * - Embedded chooser
                                                  intvec_t * - in/out: selected lines
                                                    (NB: this field takes two args)
  t - multi line text control                     textctrl_info_t *
  b - dropdown list                               qstrvec_t * - the list of items
                                                  int/qstring - the preselected item
                                                    (a qstring when the combo is editable)


//------------------------------------------------------------------------
/* Format string for AskUsingForm_c()/OpenForm_c()

  The following keywords might appear at the beginning of the form
  (case insensitive):

  STARTITEM number

    where number is a number of input field the cursor will stand on.
    By default the cursor is in the first field of the dialog box.
    The input fields are numbered from 0 (the first field is field 0).

  BUTTON name caption

    Alternative caption for a button. It may contain the character
    to highlight in this form:  ~Y~es
    Valid button names are: YES, NO, CANCEL
    For example:
        BUTTON YES Please do
        BUTTON NO Nope
        BUTTON CANCEL NONE

    By default the NO button is not displayed. If it is displayed, then
    the return value of the function will be different!
    (see the function description)

    Empty text means that there won't be any corresponding button.
    (you may also use NONE as the caption to hide it)

    A * after the button name means that this button will be the default:

      BUTTON CANCEL* Cancel

  Next, if the dialog box is kept in IDA.HLP, the following may appear:
  (this defines help context for the whole dialog box)

  @hlpMessageName[]

  If the form is not in IDA.HLP file, then it may have a built-in
  help message. In this case the help screen should be enclosed in the
  following keywords:

  HELP
  ....
  ....
  ....
  ENDHELP

  Each keyword should be alone on a line.

  Next there must be the title line and 2 empty lines.
  All text in the dialog box text string is copied to the dialog without
  any modifications. There are two special cases:

        - format parameters
        - input fields

  Format parameters are used to specify variant parts of dialog box.
  They are specified as "%x" combination, where x is format specifier.
  All input field types (except B and K) are valid format specifiers.
  List of input field types is given below. Parameter for "%x" combination
  is taken from the list function input arguments (va_list). The corresponding
  argument should contain pointer (sic, pointer) to the value to be converted
  and displayed. For example, dialog box:

  ------ format:
        Sample dialog box


        This is sample dialog box for %A
        using address %$

        <~E~nter value:N:32:16::>

  ------

  should be called as
                char *string = "something";
                ea_t addr = someaddr;
                uval_t answer = 0;
                int ok = AskUsingForm_c(format, string, &addr, &answer);

  The combination '%/' corresponds to a callback function that will be
  called when any of the fields is modified. The callback type is formchgcb_t.
  There can be only one such callback.

  The combination '%*' is used to store user data (void *) in the form.
  This data can be later retrieved from the formchgcb_t callback via the
  form action method get_ud().

  Input fields are represented by the following combination:

  <label:type:width:swidth:@hlp[]>

  where
        label - any text string serving as label for the input field
                the label may contain hotkey definition like this: "~O~pen"
                (O keystroke is hotkey here)
        type  - a character specifing type of input field.
                The form() function will perform initial validation of
                value specified by the user and convert it appropriately.
                See table of input field types below. The type can be followed
                by a decimal number, an input field id.
        width - decimal number specifying width of input field.
                this number may be omitted.
                For a field of type 'B' this attribute contains code generated
                when the user presses the button.
                For a field of type 'f' (path to file) this attribute specifies the dialog type:
                  0-'open file' dialog box
                  1-'save file' dialog box
                For a field of type 'b' (dropdown list) this attribute specifies the readonly attribute:
                  0   - read-only dropdown list
                  > 0 - editable dropdown list
        swidth -decimal number specifying width of visible part of input field.
                this number may be omitted.
        @hlp[]- help context for the input field. you may replace the
                help context with '::' (two colons) if you don't want to
                specify help context. The help context is a number of help
                screen from IDA.HLP file.


  Input field types                               va_list parameter
  -----------------                               -----------------

  A - ascii string                                char* at least MAXSTR size
  S - segment                                     sel_t*
  N - hex number, C notation                      uval_t*
  n - signed hex number, C notation               sval_t*
  L - default base (usually hex) number,          uint64*
      C notation
  l - default base (usually hex) number, signed,  int64*
      C notation
  M - hex number, no "0x" prefix                  uval_t*
  D - decimal number                              sval_t*
  O - octal number, C notation                    sval_t*
  Y - binary number, "0b" prefix                  sval_t*
  H - char value, C notation                      sval_t*
  $ - address                                     ea_t*
  I - ident                                       char* at least MAXNAMELEN size
  B - button                                      formcb_t
  k - txt: button (same as B)/gui: hyperlink      formcb_t
  K - color button                                bgcolor_t*
  F - path to folder                              char* at least QMAXPATH size
  f - path to file                                char* at least QMAXPATH size
  T - type declaration                            char* at least MAXSTR size
  E - chooser                                     chooser_info_t * - Embedded chooser
                                                  intvec_t * - in/out: selected lines
                                                    (NB: this field takes two args)
  t - multi line text control                     textctrl_info_t *
  b - dropdown list                               qstrvec_t * - the list of items
                                                  int/qstring - the preselected item
                                                    (a qstring when the combo is editable)

  The n, N, D, O, Y, H fields interpret the input as an IDC expression.
  M and $ fields fallback to IDC if the input can not be interpreted
  correctly.

  If the buffer for 'F' field contains filemasks and descriptions like this:
    *.exe|Executable files,*.dll|Dll files
  they will be used in the dialog box filter.

  The hint message can be specified before the label enclosed in '#':

  <#hint message#label:...>

  Radiobuttons and checkboxes are represented by:

  <label:type>
  <label:type>>         - end of block

  where valid types are C and R
  (you may use lowercase 'c' and 'r' if you need to create two radiobutton
  or checkbox groups on the same lines). The field id of the whole group
  can be specified between the brackets: <label:type>ID>

  field types           va_list parameter
  -----------           -----------------

  C - checkbox          ushort*                 bit mask of checkboxes
  R - radiobutton       ushort*                 number of radiobutton

  The box title and hint messages can be specified like this:

  <#item hint#title#box hint#label:type>

  The title and the box hint can be specified only in the first item of the box.
  If the hint doesn't exist, it should be specified as an empty hint (##title##)
  The subsequent items can have an item hint only:

  <#item hint#label:type>

  Initial values of input fields are specified in the corresponding
  input/output parameters (taken from va_list array).

  Ok, Cancel and (possibly) Help buttons are displayed at the bottom of
  the dialog box automatically. Their captions can be changed by the keywords
  described at the beginning of this page.

  Input field definition examples:

   <Kernel analyzer options ~1~:B:0:::>
   <~A~nalysis enabled:C>
   <~I~ndicator enabled:C>>
   <Names pre~f~ix  :A:15:15::>
   <~O~utput file:f:1:64::>
   <~O~utput directory:F:1:64::>

  Resizable fields can be separated by splitter (GUI  only) represented by <|>
  Splitter usage example:
   <~Chooser~:E1:0:40:::><|><~E~ditor:t2:0:40:::>

*/

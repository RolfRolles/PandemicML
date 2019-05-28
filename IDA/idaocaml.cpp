#define USE_DANGEROUS_FUNCTIONS
#include <ida.hpp>
#include <idp.hpp>
#include <loader.hpp>
#include <expr.hpp>

extern "C" {
#include "ocamlside.h" 
};

#include "idagraphviewer.hpp"
//#include "idatracer.hpp"

int __stdcall init(void)
{
  return PLUGIN_OK;
}

char *last_cmd_entered = NULL, *this_cmd = NULL;

void __stdcall term(void)
{
  return;
}

#define OCAML_BUFFER_SIZE 4096*10
#define OCAML_OUTGOING_BUFFER_SIZE 10000

extern "C" {
char *IDACpp_CmdToExecute = NULL;
unsigned long IDACpp_incoming_buffer_len = OCAML_BUFFER_SIZE, IDACpp_incoming_buffer_pos = 0;
char IDACpp_FinishedLastCommand = 0;
char *IDACpp_outgoing_buffer = NULL;
unsigned long IDACpp_outgoing_buffer_len = OCAML_OUTGOING_BUFFER_SIZE, IDACpp_outgoing_buffer_pos = 0;
};

static const char  IDAOCamlCallback_Args[] = { VT_LONG, 0 };
static error_t idaapi IDAOCamlCallback(value_t *argv, value_t *res)
{
  IDAOCaml_invoke_hotkey_callback(argv[0].num);
  res->num = 1;
  return eOk;
}

void InitOCamlStuff()
{
  last_cmd_entered = new char[OCAML_BUFFER_SIZE+3];
  this_cmd         = new char[OCAML_BUFFER_SIZE+3];
  IDACpp_outgoing_buffer = new char[OCAML_OUTGOING_BUFFER_SIZE+1];

  last_cmd_entered[0] = '\0';
  this_cmd[0] = '\0';
  IDACpp_outgoing_buffer[0] = '\0';

  CPPInitGraphViewer();
  // CPPInitTracer();
  set_idc_func("OCamlCallback", IDAOCamlCallback, IDAOCamlCallback_Args);

  char *argv[2];
  argv[0] = "DUMMY";
  argv[1] = NULL;
  IDAOCaml_init_interpreter(argv);
}

static bool bInitialized = false;

void __stdcall run(int arg)
{
  if (arg == -1)
  {
    PLUGIN.flags |= PLUGIN_UNL;
    msg("Unloading OCaml plugin...\n");
    return;
  }

  if(!bInitialized)
  {
    InitOCamlStuff();
    bInitialized = true;
  }
  
  char *ans = asktext(OCAML_BUFFER_SIZE, this_cmd, last_cmd_entered, "Enter OCaml statements");
  if(ans)
  {
    IDACpp_CmdToExecute = this_cmd;
    IDACpp_FinishedLastCommand = 0;
    IDAOCaml_invoke_interpreter();
    char *temp = last_cmd_entered;
    last_cmd_entered = this_cmd;
    this_cmd = temp;
  }
}

char comment[] 	= "OCaml interpreter";
char help[] 	= "";
                                                                  
/* Plugin name listed in (Edit | Plugins) */                      
char wanted_name[] = "OCaml";                             
                                                                  
/* plug-in hotkey */                                              
char wanted_hotkey[] = "CTRL-F10";                                     
                                                                  
/* defines the plugins interface to IDA */                        
plugin_t PLUGIN =                                                 
{                                                                 
  IDP_INTERFACE_VERSION,                                          
  0,              // plugin flags                                 
  init,           // initialize                                   
  term,           // terminate. this pointer may be NULL.         
  run,            // invoke plugin                                
  comment,        // comment about the plugin                     
  help,           // multiline help about the plugin              
  wanted_name,    // the preferred short name of the plugin       
  wanted_hotkey   // the preferred hotkey to run the plugin       
};


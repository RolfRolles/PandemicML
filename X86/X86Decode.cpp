char hasModRM[1024] = 
{
  1;1;1;1;0;0;0;0;1;1;1;1;0;0;0;0;  /* 0x00 */
  1;1;1;1;0;0;0;0;1;1;1;1;0;0;0;0;  /* 0x10 */
  1;1;1;1;0;0;0;0;1;1;1;1;0;0;0;0;  /* 0x20 */
  1;1;1;1;0;0;0;0;1;1;1;1;0;0;0;0;  /* 0x30 */
  0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;  /* 0x40 */
  0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;  /* 0x50 */
  0;0;1;1;0;0;0;0;0;1;0;1;0;0;0;0;  /* 0x60 */
  0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;  /* 0x70 */
  1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;  /* 0x80 */
  0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;  /* 0x90 */
  0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;  /* 0xA0 */
  0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;  /* 0xB0 */
  1;1;0;0;1;1;1;1;0;0;0;0;0;0;0;0;  /* 0xC0 */
  1;1;1;1;0;0;0;0;1;1;1;1;1;1;1;1;  /* 0xD0 */
  0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;  /* 0xE0 */
  0;0;0;0;0;0;1;1;0;0;0;0;0;0;1;1;  /* 0xF0 */
  1;1;1;1;0;0;0;0;0;0;0;0;0;1;0;0;  /* 0x0F 0x00 */
  1;1;1;1;1;1;1;1;1;0;0;0;0;0;0;1;  /* 0x0F 0x10 */
  1;1;1;1;0;0;0;0;1;1;1;1;1;1;1;1;  /* 0x0F 0x20 */
  0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;  /* 0x0F 0x30 */
  1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;  /* 0x0F 0x40 */
  1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;  /* 0x0F 0x50 */
  1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;  /* 0x0F 0x60 */
  1;1;1;1;1;1;1;0;1;1;0;0;1;1;1;1;  /* 0x0F 0x70 */
  0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;  /* 0x0F 0x80 */
  1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;  /* 0x0F 0x90 */
  0;0;0;1;1;1;0;0;0;0;0;1;1;1;1;1;  /* 0x0F 0xA0 */
  1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;  /* 0x0F 0xB0 */
  1;1;1;1;1;1;1;1;0;0;0;0;0;0;0;0;  /* 0x0F 0xC0 */
  1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;  /* 0x0F 0xD0 */
  1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;  /* 0x0F 0xE0 */
  1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;0;  /* 0x0F 0xF0 */
  1;1;1;1;1;1;1;1;1;1;1;1;0;0;0;0;  /* 0x0F 0x38 0x00 */
  1;0;0;0;1;1;0;1;0;0;0;0;1;1;1;0;  /* 0x0F 0x38 0x10 */
  1;1;1;1;1;1;0;0;1;1;1;1;0;0;0;0;  /* 0x0F 0x38 0x20 */
  1;1;1;1;1;1;0;1;1;1;1;1;1;1;1;1;  /* 0x0F 0x38 0x30 */
  1;1;0;0;0;0;0;0;0;0;0;0;0;0;0;0;  /* 0x0F 0x38 0x40 */
  0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;  /* 0x0F 0x38 0x50 */
  0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;  /* 0x0F 0x38 0x60 */
  0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;  /* 0x0F 0x38 0x70 */
  0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;  /* 0x0F 0x38 0x80 */
  0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;  /* 0x0F 0x38 0x90 */
  0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;  /* 0x0F 0x38 0xA0 */
  0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;  /* 0x0F 0x38 0xB0 */
  0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;  /* 0x0F 0x38 0xC0 */
  0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;  /* 0x0F 0x38 0xD0 */
  0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;  /* 0x0F 0x38 0xE0 */
  1;1;0;0;0;0;0;0;0;0;0;0;0;0;0;0;  /* 0x0F 0x38 0xF0 */
  0;0;0;0;0;0;0;0;1;1;1;1;1;1;1;1;  /* 0x0F 0x3A 0x00 */
  0;0;0;0;1;1;1;1;0;0;0;0;0;0;0;0;  /* 0x0F 0x3A 0x10 */
  1;1;1;0;0;0;0;0;0;0;0;0;0;0;0;0;  /* 0x0F 0x3A 0x20 */
  0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;  /* 0x0F 0x3A 0x30 */
  1;1;1;0;0;0;0;0;0;0;0;0;0;0;0;0;  /* 0x0F 0x3A 0x40 */
  0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;  /* 0x0F 0x3A 0x50 */
  1;1;1;1;0;0;0;0;0;0;0;0;0;0;0;0;  /* 0x0F 0x3A 0x60 */
  0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;  /* 0x0F 0x3A 0x70 */
  0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;  /* 0x0F 0x3A 0x80 */
  0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;  /* 0x0F 0x3A 0x90 */
  0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;  /* 0x0F 0x3A 0xA0 */
  0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;  /* 0x0F 0x3A 0xB0 */
  0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;  /* 0x0F 0x3A 0xC0 */
  0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;  /* 0x0F 0x3A 0xD0 */
  0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;  /* 0x0F 0x3A 0xE0 */
  1;1;0;0;0;0;0;0;0;0;0;0;0;0;0;0;  /* 0x0F 0x3A 0xF0 */
  
};

  case 0x0000:
  {
    // add
  }
  break;

  case 0x0001:
  {
    // add
  }
  break;

  case 0x0002:
  {
    // add
  }
  break;

  case 0x0003:
  {
    // add
  }
  break;

  case 0x0004:
  {
    // add
  }
  break;

  case 0x0005:
  {
    // add
  }
  break;

  case 0x0006:
  {
    // push
  }
  break;

  case 0x0007:
  {
    // pop
  }
  break;

  case 0x0008:
  {
    // or
  }
  break;

  case 0x0009:
  {
    // or
  }
  break;

  case 0x000a:
  {
    // or
  }
  break;

  case 0x000b:
  {
    // or
  }
  break;

  case 0x000c:
  {
    // or
  }
  break;

  case 0x000d:
  {
    // or
  }
  break;

  case 0x000e:
  {
    // push
  }
  break;

  case 0x000f:
  {
    // fatal
  }
  break;

  case 0x0010:
  {
    // adc
  }
  break;

  case 0x0011:
  {
    // adc
  }
  break;

  case 0x0012:
  {
    // adc
  }
  break;

  case 0x0013:
  {
    // adc
  }
  break;

  case 0x0014:
  {
    // adc
  }
  break;

  case 0x0015:
  {
    // adc
  }
  break;

  case 0x0016:
  {
    // push
  }
  break;

  case 0x0017:
  {
    // pop
  }
  break;

  case 0x0018:
  {
    // sbb
  }
  break;

  case 0x0019:
  {
    // sbb
  }
  break;

  case 0x001a:
  {
    // sbb
  }
  break;

  case 0x001b:
  {
    // sbb
  }
  break;

  case 0x001c:
  {
    // sbb
  }
  break;

  case 0x001d:
  {
    // sbb
  }
  break;

  case 0x001e:
  {
    // push
  }
  break;

  case 0x001f:
  {
    // pop
  }
  break;

  case 0x0020:
  {
    // and
  }
  break;

  case 0x0021:
  {
    // and
  }
  break;

  case 0x0022:
  {
    // and
  }
  break;

  case 0x0023:
  {
    // and
  }
  break;

  case 0x0024:
  {
    // and
  }
  break;

  case 0x0025:
  {
    // and
  }
  break;

  case 0x0026:
  {
    // fatal
  }
  break;

  case 0x0027:
  {
    // daa
  }
  break;

  case 0x0028:
  {
    // sub
  }
  break;

  case 0x0029:
  {
    // sub
  }
  break;

  case 0x002a:
  {
    // sub
  }
  break;

  case 0x002b:
  {
    // sub
  }
  break;

  case 0x002c:
  {
    // sub
  }
  break;

  case 0x002d:
  {
    // sub
  }
  break;

  case 0x002e:
  {
    // fatal
  }
  break;

  case 0x002f:
  {
    // das
  }
  break;

  case 0x0030:
  {
    // xor
  }
  break;

  case 0x0031:
  {
    // xor
  }
  break;

  case 0x0032:
  {
    // xor
  }
  break;

  case 0x0033:
  {
    // xor
  }
  break;

  case 0x0034:
  {
    // xor
  }
  break;

  case 0x0035:
  {
    // xor
  }
  break;

  case 0x0036:
  {
    // fatal
  }
  break;

  case 0x0037:
  {
    // aaa
  }
  break;

  case 0x0038:
  {
    // cmp
  }
  break;

  case 0x0039:
  {
    // cmp
  }
  break;

  case 0x003a:
  {
    // cmp
  }
  break;

  case 0x003b:
  {
    // cmp
  }
  break;

  case 0x003c:
  {
    // cmp
  }
  break;

  case 0x003d:
  {
    // cmp
  }
  break;

  case 0x003e:
  {
    // fatal
  }
  break;

  case 0x003f:
  {
    // aas
  }
  break;

  case 0x0040:
  {
    // inc
  }
  break;

  case 0x0041:
  {
    // inc
  }
  break;

  case 0x0042:
  {
    // inc
  }
  break;

  case 0x0043:
  {
    // inc
  }
  break;

  case 0x0044:
  {
    // inc
  }
  break;

  case 0x0045:
  {
    // inc
  }
  break;

  case 0x0046:
  {
    // inc
  }
  break;

  case 0x0047:
  {
    // inc
  }
  break;

  case 0x0048:
  {
    // dec
  }
  break;

  case 0x0049:
  {
    // dec
  }
  break;

  case 0x004a:
  {
    // dec
  }
  break;

  case 0x004b:
  {
    // dec
  }
  break;

  case 0x004c:
  {
    // dec
  }
  break;

  case 0x004d:
  {
    // dec
  }
  break;

  case 0x004e:
  {
    // dec
  }
  break;

  case 0x004f:
  {
    // dec
  }
  break;

  case 0x0050:
  {
    // push
  }
  break;

  case 0x0051:
  {
    // push
  }
  break;

  case 0x0052:
  {
    // push
  }
  break;

  case 0x0053:
  {
    // push
  }
  break;

  case 0x0054:
  {
    // push
  }
  break;

  case 0x0055:
  {
    // push
  }
  break;

  case 0x0056:
  {
    // push
  }
  break;

  case 0x0057:
  {
    // push
  }
  break;

  case 0x0058:
  {
    // pop
  }
  break;

  case 0x0059:
  {
    // pop
  }
  break;

  case 0x005a:
  {
    // pop
  }
  break;

  case 0x005b:
  {
    // pop
  }
  break;

  case 0x005c:
  {
    // pop
  }
  break;

  case 0x005d:
  {
    // pop
  }
  break;

  case 0x005e:
  {
    // pop
  }
  break;

  case 0x005f:
  {
    // pop
  }
  break;

  case 0x0060:
  {
    int opsize = determine_operand_size(config, &prefixcontext, &modrmcontext);
    switch(opsize)
    {
      case 16:
      {
        // pushaw
      }
      break;
      
      case 32:
      {
        // pushad
      }
      break;
      
    }
  }
  break;

  case 0x0061:
  {
    int opsize = determine_operand_size(config, &prefixcontext, &modrmcontext);
    switch(opsize)
    {
      case 16:
      {
        // popaw
      }
      break;
      
      case 32:
      {
        // popad
      }
      break;
      
    }
  }
  break;

  case 0x0062:
  {
    // bound
  }
  break;

  case 0x0063:
  {
    // arpl
  }
  break;

  case 0x0064:
  {
    // fatal
  }
  break;

  case 0x0065:
  {
    // fatal
  }
  break;

  case 0x0066:
  {
    // fatal
  }
  break;

  case 0x0067:
  {
    // fatal
  }
  break;

  case 0x0068:
  {
    // push
  }
  break;

  case 0x0069:
  {
    // imul
  }
  break;

  case 0x006a:
  {
    // push
  }
  break;

  case 0x006b:
  {
    // imul
  }
  break;

  case 0x006c:
  {
    // insb
  }
  break;

  case 0x006d:
  {
    int opsize = determine_operand_size(config, &prefixcontext, &modrmcontext);
    switch(opsize)
    {
      case 16:
      {
        // insw
      }
      break;
      
      case 32:
      {
        // insd
      }
      break;
      
    }
  }
  break;

  case 0x006e:
  {
    // outsb
  }
  break;

  case 0x006f:
  {
    int opsize = determine_operand_size(config, &prefixcontext, &modrmcontext);
    switch(opsize)
    {
      case 16:
      {
        // outsw
      }
      break;
      
      case 32:
      {
        // outsd
      }
      break;
      
    }
  }
  break;

  case 0x0070:
  {
    // jo
  }
  break;

  case 0x0071:
  {
    // jno
  }
  break;

  case 0x0072:
  {
    // jb
  }
  break;

  case 0x0073:
  {
    // jae
  }
  break;

  case 0x0074:
  {
    // jz
  }
  break;

  case 0x0075:
  {
    // jnz
  }
  break;

  case 0x0076:
  {
    // jbe
  }
  break;

  case 0x0077:
  {
    // ja
  }
  break;

  case 0x0078:
  {
    // js
  }
  break;

  case 0x0079:
  {
    // jns
  }
  break;

  case 0x007a:
  {
    // jp
  }
  break;

  case 0x007b:
  {
    // jnp
  }
  break;

  case 0x007c:
  {
    // jl
  }
  break;

  case 0x007d:
  {
    // jge
  }
  break;

  case 0x007e:
  {
    // jle
  }
  break;

  case 0x007f:
  {
    // jg
  }
  break;

  case 0x0080:
  {
    switch(modrmcontext.greg)
    {
        case 0:
        {
          // add
        }
        break;
        
        case 1:
        {
          // or
        }
        break;
        
        case 2:
        {
          // adc
        }
        break;
        
        case 3:
        {
          // sbb
        }
        break;
        
        case 4:
        {
          // and
        }
        break;
        
        case 5:
        {
          // sub
        }
        break;
        
        case 6:
        {
          // xor
        }
        break;
        
        case 7:
        {
          // cmp
        }
        break;
        
    }
  }
  break;

  case 0x0081:
  {
    switch(modrmcontext.greg)
    {
        case 0:
        {
          // add
        }
        break;
        
        case 1:
        {
          // or
        }
        break;
        
        case 2:
        {
          // adc
        }
        break;
        
        case 3:
        {
          // sbb
        }
        break;
        
        case 4:
        {
          // and
        }
        break;
        
        case 5:
        {
          // sub
        }
        break;
        
        case 6:
        {
          // xor
        }
        break;
        
        case 7:
        {
          // cmp
        }
        break;
        
    }
  }
  break;

  case 0x0082:
  {
    switch(modrmcontext.greg)
    {
        case 0:
        {
          // add
        }
        break;
        
        case 1:
        {
          // or
        }
        break;
        
        case 2:
        {
          // adc
        }
        break;
        
        case 3:
        {
          // sbb
        }
        break;
        
        case 4:
        {
          // and
        }
        break;
        
        case 5:
        {
          // sub
        }
        break;
        
        case 6:
        {
          // xor
        }
        break;
        
        case 7:
        {
          // cmp
        }
        break;
        
    }
  }
  break;

  case 0x0083:
  {
    switch(modrmcontext.greg)
    {
        case 0:
        {
          // add
        }
        break;
        
        case 1:
        {
          // or
        }
        break;
        
        case 2:
        {
          // adc
        }
        break;
        
        case 3:
        {
          // sbb
        }
        break;
        
        case 4:
        {
          // and
        }
        break;
        
        case 5:
        {
          // sub
        }
        break;
        
        case 6:
        {
          // xor
        }
        break;
        
        case 7:
        {
          // cmp
        }
        break;
        
    }
  }
  break;

  case 0x0084:
  {
    // test
  }
  break;

  case 0x0085:
  {
    // test
  }
  break;

  case 0x0086:
  {
    // xchg
  }
  break;

  case 0x0087:
  {
    // xchg
  }
  break;

  case 0x0088:
  {
    // mov
  }
  break;

  case 0x0089:
  {
    // mov
  }
  break;

  case 0x008a:
  {
    // mov
  }
  break;

  case 0x008b:
  {
    // mov
  }
  break;

  case 0x008c:
  {
    // mov
  }
  break;

  case 0x008d:
  {
    // lea
  }
  break;

  case 0x008e:
  {
    // mov
  }
  break;

  case 0x008f:
  {
    switch(modrmcontext.greg)
    {
        case 0:
        {
          // pop
        }
        break;
        
        case 1:
        {
          // invalid
        }
        break;
        
        case 2:
        {
          // invalid
        }
        break;
        
        case 3:
        {
          // invalid
        }
        break;
        
        case 4:
        {
          // invalid
        }
        break;
        
        case 5:
        {
          // invalid
        }
        break;
        
        case 6:
        {
          // invalid
        }
        break;
        
        case 7:
        {
          // invalid
        }
        break;
        
    }
  }
  break;

  case 0x0090:
  {
    // SSE/all
  }
  break;

  case 0x0091:
  {
    // xchg
  }
  break;

  case 0x0092:
  {
    // xchg
  }
  break;

  case 0x0093:
  {
    // xchg
  }
  break;

  case 0x0094:
  {
    // xchg
  }
  break;

  case 0x0095:
  {
    // xchg
  }
  break;

  case 0x0096:
  {
    // xchg
  }
  break;

  case 0x0097:
  {
    // xchg
  }
  break;

  case 0x0098:
  {
    int opsize = determine_operand_size(config, &prefixcontext, &modrmcontext);
    switch(opsize)
    {
      case 16:
      {
        // cbw
      }
      break;
      
      case 32:
      {
        // cwde
      }
      break;
      
    }
  }
  break;

  case 0x0099:
  {
    int opsize = determine_operand_size(config, &prefixcontext, &modrmcontext);
    switch(opsize)
    {
      case 16:
      {
        // cwd
      }
      break;
      
      case 32:
      {
        // cdq
      }
      break;
      
    }
  }
  break;

  case 0x009a:
  {
    // callF
  }
  break;

  case 0x009b:
  {
    // wait
  }
  break;

  case 0x009c:
  {
    int opsize = determine_operand_size(config, &prefixcontext, &modrmcontext);
    switch(opsize)
    {
      case 16:
      {
        // pushfw
      }
      break;
      
      case 32:
      {
        // pushfd
      }
      break;
      
    }
  }
  break;

  case 0x009d:
  {
    int opsize = determine_operand_size(config, &prefixcontext, &modrmcontext);
    switch(opsize)
    {
      case 16:
      {
        // popfw
      }
      break;
      
      case 32:
      {
        // popfd
      }
      break;
      
    }
  }
  break;

  case 0x009e:
  {
    // sahf
  }
  break;

  case 0x009f:
  {
    // lahf
  }
  break;

  case 0x00a0:
  {
    // mov
  }
  break;

  case 0x00a1:
  {
    // mov
  }
  break;

  case 0x00a2:
  {
    // mov
  }
  break;

  case 0x00a3:
  {
    // mov
  }
  break;

  case 0x00a4:
  {
    // movsb
  }
  break;

  case 0x00a5:
  {
    int opsize = determine_operand_size(config, &prefixcontext, &modrmcontext);
    switch(opsize)
    {
      case 16:
      {
        // movsw
      }
      break;
      
      case 32:
      {
        // movsd
      }
      break;
      
    }
  }
  break;

  case 0x00a6:
  {
    // cmpsb
  }
  break;

  case 0x00a7:
  {
    int opsize = determine_operand_size(config, &prefixcontext, &modrmcontext);
    switch(opsize)
    {
      case 16:
      {
        // cmpsw
      }
      break;
      
      case 32:
      {
        // cmpsd
      }
      break;
      
    }
  }
  break;

  case 0x00a8:
  {
    // test
  }
  break;

  case 0x00a9:
  {
    // test
  }
  break;

  case 0x00aa:
  {
    // stosb
  }
  break;

  case 0x00ab:
  {
    int opsize = determine_operand_size(config, &prefixcontext, &modrmcontext);
    switch(opsize)
    {
      case 16:
      {
        // stosw
      }
      break;
      
      case 32:
      {
        // stosd
      }
      break;
      
    }
  }
  break;

  case 0x00ac:
  {
    // lodsb
  }
  break;

  case 0x00ad:
  {
    int opsize = determine_operand_size(config, &prefixcontext, &modrmcontext);
    switch(opsize)
    {
      case 16:
      {
        // lodsw
      }
      break;
      
      case 32:
      {
        // lodsd
      }
      break;
      
    }
  }
  break;

  case 0x00ae:
  {
    // scasb
  }
  break;

  case 0x00af:
  {
    int opsize = determine_operand_size(config, &prefixcontext, &modrmcontext);
    switch(opsize)
    {
      case 16:
      {
        // scasw
      }
      break;
      
      case 32:
      {
        // scasd
      }
      break;
      
    }
  }
  break;

  case 0x00b0:
  {
    // mov
  }
  break;

  case 0x00b1:
  {
    // mov
  }
  break;

  case 0x00b2:
  {
    // mov
  }
  break;

  case 0x00b3:
  {
    // mov
  }
  break;

  case 0x00b4:
  {
    // mov
  }
  break;

  case 0x00b5:
  {
    // mov
  }
  break;

  case 0x00b6:
  {
    // mov
  }
  break;

  case 0x00b7:
  {
    // mov
  }
  break;

  case 0x00b8:
  {
    // mov
  }
  break;

  case 0x00b9:
  {
    // mov
  }
  break;

  case 0x00ba:
  {
    // mov
  }
  break;

  case 0x00bb:
  {
    // mov
  }
  break;

  case 0x00bc:
  {
    // mov
  }
  break;

  case 0x00bd:
  {
    // mov
  }
  break;

  case 0x00be:
  {
    // mov
  }
  break;

  case 0x00bf:
  {
    // mov
  }
  break;

  case 0x00c0:
  {
    switch(modrmcontext.greg)
    {
        case 0:
        {
          // rol
        }
        break;
        
        case 1:
        {
          // ror
        }
        break;
        
        case 2:
        {
          // rcl
        }
        break;
        
        case 3:
        {
          // rcr
        }
        break;
        
        case 4:
        {
          // shl
        }
        break;
        
        case 5:
        {
          // shr
        }
        break;
        
        case 6:
        {
          // sal
        }
        break;
        
        case 7:
        {
          // sar
        }
        break;
        
    }
  }
  break;

  case 0x00c1:
  {
    switch(modrmcontext.greg)
    {
        case 0:
        {
          // rol
        }
        break;
        
        case 1:
        {
          // ror
        }
        break;
        
        case 2:
        {
          // rcl
        }
        break;
        
        case 3:
        {
          // rcr
        }
        break;
        
        case 4:
        {
          // shl
        }
        break;
        
        case 5:
        {
          // shr
        }
        break;
        
        case 6:
        {
          // sal
        }
        break;
        
        case 7:
        {
          // sar
        }
        break;
        
    }
  }
  break;

  case 0x00c2:
  {
    // ret
  }
  break;

  case 0x00c3:
  {
    // ret
  }
  break;

  case 0x00c4:
  {
    // les
  }
  break;

  case 0x00c5:
  {
    // lds
  }
  break;

  case 0x00c6:
  {
    switch(modrmcontext.greg)
    {
        case 0:
        {
          // mov
        }
        break;
        
        case 1:
        {
          // invalid
        }
        break;
        
        case 2:
        {
          // invalid
        }
        break;
        
        case 3:
        {
          // invalid
        }
        break;
        
        case 4:
        {
          // invalid
        }
        break;
        
        case 5:
        {
          // invalid
        }
        break;
        
        case 6:
        {
          // invalid
        }
        break;
        
        case 7:
        {
          // invalid
        }
        break;
        
    }
  }
  break;

  case 0x00c7:
  {
    switch(modrmcontext.greg)
    {
        case 0:
        {
          // mov
        }
        break;
        
        case 1:
        {
          // invalid
        }
        break;
        
        case 2:
        {
          // invalid
        }
        break;
        
        case 3:
        {
          // invalid
        }
        break;
        
        case 4:
        {
          // invalid
        }
        break;
        
        case 5:
        {
          // invalid
        }
        break;
        
        case 6:
        {
          // invalid
        }
        break;
        
        case 7:
        {
          // invalid
        }
        break;
        
    }
  }
  break;

  case 0x00c8:
  {
    // enter
  }
  break;

  case 0x00c9:
  {
    // leave
  }
  break;

  case 0x00ca:
  {
    // retf
  }
  break;

  case 0x00cb:
  {
    // retf
  }
  break;

  case 0x00cc:
  {
    // int3
  }
  break;

  case 0x00cd:
  {
    // int
  }
  break;

  case 0x00ce:
  {
    // into
  }
  break;

  case 0x00cf:
  {
    int opsize = determine_operand_size(config, &prefixcontext, &modrmcontext);
    switch(opsize)
    {
      case 16:
      {
        // iretw
      }
      break;
      
      case 32:
      {
        // iretd
      }
      break;
      
    }
  }
  break;

  case 0x00d0:
  {
    switch(modrmcontext.greg)
    {
        case 0:
        {
          // rol
        }
        break;
        
        case 1:
        {
          // ror
        }
        break;
        
        case 2:
        {
          // rcl
        }
        break;
        
        case 3:
        {
          // rcr
        }
        break;
        
        case 4:
        {
          // shl
        }
        break;
        
        case 5:
        {
          // shr
        }
        break;
        
        case 6:
        {
          // sal
        }
        break;
        
        case 7:
        {
          // sar
        }
        break;
        
    }
  }
  break;

  case 0x00d1:
  {
    switch(modrmcontext.greg)
    {
        case 0:
        {
          // rol
        }
        break;
        
        case 1:
        {
          // ror
        }
        break;
        
        case 2:
        {
          // rcl
        }
        break;
        
        case 3:
        {
          // rcr
        }
        break;
        
        case 4:
        {
          // shl
        }
        break;
        
        case 5:
        {
          // shr
        }
        break;
        
        case 6:
        {
          // sal
        }
        break;
        
        case 7:
        {
          // sar
        }
        break;
        
    }
  }
  break;

  case 0x00d2:
  {
    switch(modrmcontext.greg)
    {
        case 0:
        {
          // rol
        }
        break;
        
        case 1:
        {
          // ror
        }
        break;
        
        case 2:
        {
          // rcl
        }
        break;
        
        case 3:
        {
          // rcr
        }
        break;
        
        case 4:
        {
          // shl
        }
        break;
        
        case 5:
        {
          // shr
        }
        break;
        
        case 6:
        {
          // sal
        }
        break;
        
        case 7:
        {
          // sar
        }
        break;
        
    }
  }
  break;

  case 0x00d3:
  {
    switch(modrmcontext.greg)
    {
        case 0:
        {
          // rol
        }
        break;
        
        case 1:
        {
          // ror
        }
        break;
        
        case 2:
        {
          // rcl
        }
        break;
        
        case 3:
        {
          // rcr
        }
        break;
        
        case 4:
        {
          // shl
        }
        break;
        
        case 5:
        {
          // shr
        }
        break;
        
        case 6:
        {
          // sal
        }
        break;
        
        case 7:
        {
          // sar
        }
        break;
        
    }
  }
  break;

  case 0x00d4:
  {
    // aam
  }
  break;

  case 0x00d5:
  {
    // aad
  }
  break;

  case 0x00d6:
  {
    // salc
  }
  break;

  case 0x00d7:
  {
    int addrsize = determine_address_size(config, &prefixcontext);
    switch(addrsize)
    {
      case 16:
      {
        // xlat
      }
      break;
      
      case 32:
      {
        // xlat
      }
      break;
      
    }
  }
  break;

  case 0x00d8:
  {
    switch(modrmcontext.byte >> 6)
    {
      case 3:
      {
        switch(modrmcontext.greg)
        {
            case 0:
            {
              // fadd
            }
            break;
            
            case 1:
            {
              // fmul
            }
            break;
            
            case 2:
            {
              // fcom
            }
            break;
            
            case 3:
            {
              // fcomp
            }
            break;
            
            case 4:
            {
              // fsub
            }
            break;
            
            case 5:
            {
              // fsubr
            }
            break;
            
            case 6:
            {
              // fdiv
            }
            break;
            
            case 7:
            {
              // fdivr
            }
            break;
            
        }
      }
      break;
      
      default:
      {
        switch(modrmcontext.greg)
        {
            case 0:
            {
              // fadd
            }
            break;
            
            case 1:
            {
              // fmul
            }
            break;
            
            case 2:
            {
              // fcom
            }
            break;
            
            case 3:
            {
              // fcomp
            }
            break;
            
            case 4:
            {
              // fsub
            }
            break;
            
            case 5:
            {
              // fsubr
            }
            break;
            
            case 6:
            {
              // fdiv
            }
            break;
            
            case 7:
            {
              // fdivr
            }
            break;
            
        }
      }
      break;
      
    }
  }
  break;

  case 0x00d9:
  {
    switch(modrmcontext.byte >> 6)
    {
      case 3:
      {
        switch(modrmcontext.greg)
        {
            case 0:
            {
              // fld
            }
            break;
            
            case 1:
            {
              // fxch
            }
            break;
            
            case 2:
            {
              switch(modrmcontext.greg)
              {
                  case 0:
                  {
                    // fnop
                  }
                  break;
                  
                  case 1:
                  {
                    // invalid
                  }
                  break;
                  
                  case 2:
                  {
                    // invalid
                  }
                  break;
                  
                  case 3:
                  {
                    // invalid
                  }
                  break;
                  
                  case 4:
                  {
                    // invalid
                  }
                  break;
                  
                  case 5:
                  {
                    // invalid
                  }
                  break;
                  
                  case 6:
                  {
                    // invalid
                  }
                  break;
                  
                  case 7:
                  {
                    // invalid
                  }
                  break;
                  
              }
            }
            break;
            
            case 3:
            {
              // invalid
            }
            break;
            
            case 4:
            {
              switch(modrmcontext.greg)
              {
                  case 0:
                  {
                    // fchs
                  }
                  break;
                  
                  case 1:
                  {
                    // fabs
                  }
                  break;
                  
                  case 2:
                  {
                    // invalid
                  }
                  break;
                  
                  case 3:
                  {
                    // invalid
                  }
                  break;
                  
                  case 4:
                  {
                    // ftst
                  }
                  break;
                  
                  case 5:
                  {
                    // fxam
                  }
                  break;
                  
                  case 6:
                  {
                    // invalid
                  }
                  break;
                  
                  case 7:
                  {
                    // invalid
                  }
                  break;
                  
              }
            }
            break;
            
            case 5:
            {
              switch(modrmcontext.greg)
              {
                  case 0:
                  {
                    // fld1
                  }
                  break;
                  
                  case 1:
                  {
                    // fldl2t
                  }
                  break;
                  
                  case 2:
                  {
                    // fldl2e
                  }
                  break;
                  
                  case 3:
                  {
                    // fldpi
                  }
                  break;
                  
                  case 4:
                  {
                    // fldlg2
                  }
                  break;
                  
                  case 5:
                  {
                    // fldln2
                  }
                  break;
                  
                  case 6:
                  {
                    // fldz
                  }
                  break;
                  
                  case 7:
                  {
                    // invalid
                  }
                  break;
                  
              }
            }
            break;
            
            case 6:
            {
              switch(modrmcontext.greg)
              {
                  case 0:
                  {
                    // f2xm1
                  }
                  break;
                  
                  case 1:
                  {
                    // fyl2x
                  }
                  break;
                  
                  case 2:
                  {
                    // fptan
                  }
                  break;
                  
                  case 3:
                  {
                    // fpatan
                  }
                  break;
                  
                  case 4:
                  {
                    // fxtract
                  }
                  break;
                  
                  case 5:
                  {
                    // fprem1
                  }
                  break;
                  
                  case 6:
                  {
                    // fdecstp
                  }
                  break;
                  
                  case 7:
                  {
                    // fincstp
                  }
                  break;
                  
              }
            }
            break;
            
            case 7:
            {
              switch(modrmcontext.greg)
              {
                  case 0:
                  {
                    // fprem
                  }
                  break;
                  
                  case 1:
                  {
                    // fyl2xp1
                  }
                  break;
                  
                  case 2:
                  {
                    // fsqrt
                  }
                  break;
                  
                  case 3:
                  {
                    // fsincos
                  }
                  break;
                  
                  case 4:
                  {
                    // frndint
                  }
                  break;
                  
                  case 5:
                  {
                    // fscale
                  }
                  break;
                  
                  case 6:
                  {
                    // fsin
                  }
                  break;
                  
                  case 7:
                  {
                    // fcos
                  }
                  break;
                  
              }
            }
            break;
            
        }
      }
      break;
      
      default:
      {
        switch(modrmcontext.greg)
        {
            case 0:
            {
              // fld
            }
            break;
            
            case 1:
            {
              // invalid
            }
            break;
            
            case 2:
            {
              // fst
            }
            break;
            
            case 3:
            {
              // fstp
            }
            break;
            
            case 4:
            {
              // fldenv
            }
            break;
            
            case 5:
            {
              // fldcw
            }
            break;
            
            case 6:
            {
              // fstenv
            }
            break;
            
            case 7:
            {
              // fstcw
            }
            break;
            
        }
      }
      break;
      
    }
  }
  break;

  case 0x00da:
  {
    switch(modrmcontext.byte >> 6)
    {
      case 3:
      {
        switch(modrmcontext.greg)
        {
            case 0:
            {
              // fcmovb
            }
            break;
            
            case 1:
            {
              // fcmove
            }
            break;
            
            case 2:
            {
              // fcmovbe
            }
            break;
            
            case 3:
            {
              // fcmovu
            }
            break;
            
            case 4:
            {
              // invalid
            }
            break;
            
            case 5:
            {
              switch(modrmcontext.greg)
              {
                  case 0:
                  {
                    // invalid
                  }
                  break;
                  
                  case 1:
                  {
                    // fucompp
                  }
                  break;
                  
                  case 2:
                  {
                    // invalid
                  }
                  break;
                  
                  case 3:
                  {
                    // invalid
                  }
                  break;
                  
                  case 4:
                  {
                    // invalid
                  }
                  break;
                  
                  case 5:
                  {
                    // invalid
                  }
                  break;
                  
                  case 6:
                  {
                    // invalid
                  }
                  break;
                  
                  case 7:
                  {
                    // invalid
                  }
                  break;
                  
              }
            }
            break;
            
            case 6:
            {
              // invalid
            }
            break;
            
            case 7:
            {
              // invalid
            }
            break;
            
        }
      }
      break;
      
      default:
      {
        switch(modrmcontext.greg)
        {
            case 0:
            {
              // fiadd
            }
            break;
            
            case 1:
            {
              // fimul
            }
            break;
            
            case 2:
            {
              // ficom
            }
            break;
            
            case 3:
            {
              // ficomp
            }
            break;
            
            case 4:
            {
              // fisub
            }
            break;
            
            case 5:
            {
              // fisubr
            }
            break;
            
            case 6:
            {
              // fidiv
            }
            break;
            
            case 7:
            {
              // fidivr
            }
            break;
            
        }
      }
      break;
      
    }
  }
  break;

  case 0x00db:
  {
    switch(modrmcontext.byte >> 6)
    {
      case 3:
      {
        switch(modrmcontext.greg)
        {
            case 0:
            {
              // fcmovnb
            }
            break;
            
            case 1:
            {
              // fcmovne
            }
            break;
            
            case 2:
            {
              // fcmovnbe
            }
            break;
            
            case 3:
            {
              // fcmovnu
            }
            break;
            
            case 4:
            {
              switch(modrmcontext.greg)
              {
                  case 0:
                  {
                    // invalid
                  }
                  break;
                  
                  case 1:
                  {
                    // invalid
                  }
                  break;
                  
                  case 2:
                  {
                    // fclex
                  }
                  break;
                  
                  case 3:
                  {
                    // finit
                  }
                  break;
                  
                  case 4:
                  {
                    // invalid
                  }
                  break;
                  
                  case 5:
                  {
                    // invalid
                  }
                  break;
                  
                  case 6:
                  {
                    // invalid
                  }
                  break;
                  
                  case 7:
                  {
                    // invalid
                  }
                  break;
                  
              }
            }
            break;
            
            case 5:
            {
              // fucomi
            }
            break;
            
            case 6:
            {
              // fcomi
            }
            break;
            
            case 7:
            {
              // invalid
            }
            break;
            
        }
      }
      break;
      
      default:
      {
        switch(modrmcontext.greg)
        {
            case 0:
            {
              // fild
            }
            break;
            
            case 1:
            {
              // fisttp
            }
            break;
            
            case 2:
            {
              // fist
            }
            break;
            
            case 3:
            {
              // fistp
            }
            break;
            
            case 4:
            {
              // invalid
            }
            break;
            
            case 5:
            {
              // fld
            }
            break;
            
            case 6:
            {
              // invalid
            }
            break;
            
            case 7:
            {
              // fstp
            }
            break;
            
        }
      }
      break;
      
    }
  }
  break;

  case 0x00dc:
  {
    switch(modrmcontext.byte >> 6)
    {
      case 3:
      {
        switch(modrmcontext.greg)
        {
            case 0:
            {
              // fadd
            }
            break;
            
            case 1:
            {
              // fmul
            }
            break;
            
            case 2:
            {
              // invalid
            }
            break;
            
            case 3:
            {
              // invalid
            }
            break;
            
            case 4:
            {
              // fsub
            }
            break;
            
            case 5:
            {
              // fsubr
            }
            break;
            
            case 6:
            {
              // fdiv
            }
            break;
            
            case 7:
            {
              // fdivr
            }
            break;
            
        }
      }
      break;
      
      default:
      {
        switch(modrmcontext.greg)
        {
            case 0:
            {
              // fadd
            }
            break;
            
            case 1:
            {
              // fmul
            }
            break;
            
            case 2:
            {
              // fcom
            }
            break;
            
            case 3:
            {
              // fcomp
            }
            break;
            
            case 4:
            {
              // fsub
            }
            break;
            
            case 5:
            {
              // fsubr
            }
            break;
            
            case 6:
            {
              // fdiv
            }
            break;
            
            case 7:
            {
              // fdivr
            }
            break;
            
        }
      }
      break;
      
    }
  }
  break;

  case 0x00dd:
  {
    switch(modrmcontext.byte >> 6)
    {
      case 3:
      {
        switch(modrmcontext.greg)
        {
            case 0:
            {
              // ffree
            }
            break;
            
            case 1:
            {
              // invalid
            }
            break;
            
            case 2:
            {
              // fst
            }
            break;
            
            case 3:
            {
              // fstp
            }
            break;
            
            case 4:
            {
              // fucom
            }
            break;
            
            case 5:
            {
              // fucomp
            }
            break;
            
            case 6:
            {
              // invalid
            }
            break;
            
            case 7:
            {
              // invalid
            }
            break;
            
        }
      }
      break;
      
      default:
      {
        switch(modrmcontext.greg)
        {
            case 0:
            {
              // fld
            }
            break;
            
            case 1:
            {
              // fisttp
            }
            break;
            
            case 2:
            {
              // fst
            }
            break;
            
            case 3:
            {
              // fstp
            }
            break;
            
            case 4:
            {
              // frstor
            }
            break;
            
            case 5:
            {
              // invalid
            }
            break;
            
            case 6:
            {
              // fsave
            }
            break;
            
            case 7:
            {
              // fstsw
            }
            break;
            
        }
      }
      break;
      
    }
  }
  break;

  case 0x00de:
  {
    switch(modrmcontext.byte >> 6)
    {
      case 3:
      {
        switch(modrmcontext.greg)
        {
            case 0:
            {
              // faddp
            }
            break;
            
            case 1:
            {
              // fmulp
            }
            break;
            
            case 2:
            {
              // invalid
            }
            break;
            
            case 3:
            {
              switch(modrmcontext.greg)
              {
                  case 0:
                  {
                    // invalid
                  }
                  break;
                  
                  case 1:
                  {
                    // fcompp
                  }
                  break;
                  
                  case 2:
                  {
                    // invalid
                  }
                  break;
                  
                  case 3:
                  {
                    // invalid
                  }
                  break;
                  
                  case 4:
                  {
                    // invalid
                  }
                  break;
                  
                  case 5:
                  {
                    // invalid
                  }
                  break;
                  
                  case 6:
                  {
                    // invalid
                  }
                  break;
                  
                  case 7:
                  {
                    // invalid
                  }
                  break;
                  
              }
            }
            break;
            
            case 4:
            {
              // fsubrp
            }
            break;
            
            case 5:
            {
              // fsubp
            }
            break;
            
            case 6:
            {
              // fdivrp
            }
            break;
            
            case 7:
            {
              // fdivp
            }
            break;
            
        }
      }
      break;
      
      default:
      {
        switch(modrmcontext.greg)
        {
            case 0:
            {
              // fiadd
            }
            break;
            
            case 1:
            {
              // fimul
            }
            break;
            
            case 2:
            {
              // ficom
            }
            break;
            
            case 3:
            {
              // ficomp
            }
            break;
            
            case 4:
            {
              // fisub
            }
            break;
            
            case 5:
            {
              // fisubr
            }
            break;
            
            case 6:
            {
              // fidiv
            }
            break;
            
            case 7:
            {
              // fidivr
            }
            break;
            
        }
      }
      break;
      
    }
  }
  break;

  case 0x00df:
  {
    switch(modrmcontext.byte >> 6)
    {
      case 3:
      {
        switch(modrmcontext.greg)
        {
            case 0:
            {
              // invalid
            }
            break;
            
            case 1:
            {
              // invalid
            }
            break;
            
            case 2:
            {
              // invalid
            }
            break;
            
            case 3:
            {
              // invalid
            }
            break;
            
            case 4:
            {
              switch(modrmcontext.greg)
              {
                  case 0:
                  {
                    // fstsw
                  }
                  break;
                  
                  case 1:
                  {
                    // invalid
                  }
                  break;
                  
                  case 2:
                  {
                    // invalid
                  }
                  break;
                  
                  case 3:
                  {
                    // invalid
                  }
                  break;
                  
                  case 4:
                  {
                    // invalid
                  }
                  break;
                  
                  case 5:
                  {
                    // invalid
                  }
                  break;
                  
                  case 6:
                  {
                    // invalid
                  }
                  break;
                  
                  case 7:
                  {
                    // invalid
                  }
                  break;
                  
              }
            }
            break;
            
            case 5:
            {
              // fucomip
            }
            break;
            
            case 6:
            {
              // fcomip
            }
            break;
            
            case 7:
            {
              // invalid
            }
            break;
            
        }
      }
      break;
      
      default:
      {
        switch(modrmcontext.greg)
        {
            case 0:
            {
              // fild
            }
            break;
            
            case 1:
            {
              // fisttp
            }
            break;
            
            case 2:
            {
              // fist
            }
            break;
            
            case 3:
            {
              // fistp
            }
            break;
            
            case 4:
            {
              // fbld
            }
            break;
            
            case 5:
            {
              // fild
            }
            break;
            
            case 6:
            {
              // fbstp
            }
            break;
            
            case 7:
            {
              // fistp
            }
            break;
            
        }
      }
      break;
      
    }
  }
  break;

  case 0x00e0:
  {
    // loopnz
  }
  break;

  case 0x00e1:
  {
    // loopz
  }
  break;

  case 0x00e2:
  {
    // loop
  }
  break;

  case 0x00e3:
  {
    int addrsize = determine_address_size(config, &prefixcontext);
    switch(addrsize)
    {
      case 16:
      {
        // jcxz
      }
      break;
      
      case 32:
      {
        // jecxz
      }
      break;
      
    }
  }
  break;

  case 0x00e4:
  {
    // in
  }
  break;

  case 0x00e5:
  {
    // in
  }
  break;

  case 0x00e6:
  {
    // out
  }
  break;

  case 0x00e7:
  {
    // out
  }
  break;

  case 0x00e8:
  {
    // call
  }
  break;

  case 0x00e9:
  {
    // jmp
  }
  break;

  case 0x00ea:
  {
    // jmpF
  }
  break;

  case 0x00eb:
  {
    // jmp
  }
  break;

  case 0x00ec:
  {
    // in
  }
  break;

  case 0x00ed:
  {
    // in
  }
  break;

  case 0x00ee:
  {
    // out
  }
  break;

  case 0x00ef:
  {
    // out
  }
  break;

  case 0x00f0:
  {
    // fatal
  }
  break;

  case 0x00f1:
  {
    // icebp
  }
  break;

  case 0x00f2:
  {
    // fatal
  }
  break;

  case 0x00f3:
  {
    // fatal
  }
  break;

  case 0x00f4:
  {
    // hlt
  }
  break;

  case 0x00f5:
  {
    // cmc
  }
  break;

  case 0x00f6:
  {
    switch(modrmcontext.greg)
    {
        case 0:
        {
          // test
        }
        break;
        
        case 1:
        {
          // invalid
        }
        break;
        
        case 2:
        {
          // not
        }
        break;
        
        case 3:
        {
          // neg
        }
        break;
        
        case 4:
        {
          // mul
        }
        break;
        
        case 5:
        {
          // imul
        }
        break;
        
        case 6:
        {
          // div
        }
        break;
        
        case 7:
        {
          // idiv
        }
        break;
        
    }
  }
  break;

  case 0x00f7:
  {
    switch(modrmcontext.greg)
    {
        case 0:
        {
          // test
        }
        break;
        
        case 1:
        {
          // invalid
        }
        break;
        
        case 2:
        {
          // not
        }
        break;
        
        case 3:
        {
          // neg
        }
        break;
        
        case 4:
        {
          // mul
        }
        break;
        
        case 5:
        {
          // imul
        }
        break;
        
        case 6:
        {
          // div
        }
        break;
        
        case 7:
        {
          // idiv
        }
        break;
        
    }
  }
  break;

  case 0x00f8:
  {
    // clc
  }
  break;

  case 0x00f9:
  {
    // stc
  }
  break;

  case 0x00fa:
  {
    // cli
  }
  break;

  case 0x00fb:
  {
    // sti
  }
  break;

  case 0x00fc:
  {
    // cld
  }
  break;

  case 0x00fd:
  {
    // std
  }
  break;

  case 0x00fe:
  {
    switch(modrmcontext.greg)
    {
        case 0:
        {
          // inc
        }
        break;
        
        case 1:
        {
          // dec
        }
        break;
        
        case 2:
        {
          // invalid
        }
        break;
        
        case 3:
        {
          // invalid
        }
        break;
        
        case 4:
        {
          // invalid
        }
        break;
        
        case 5:
        {
          // invalid
        }
        break;
        
        case 6:
        {
          // invalid
        }
        break;
        
        case 7:
        {
          // invalid
        }
        break;
        
    }
  }
  break;

  case 0x00ff:
  {
    switch(modrmcontext.greg)
    {
        case 0:
        {
          // inc
        }
        break;
        
        case 1:
        {
          // dec
        }
        break;
        
        case 2:
        {
          // call
        }
        break;
        
        case 3:
        {
          // callF
        }
        break;
        
        case 4:
        {
          // jmp
        }
        break;
        
        case 5:
        {
          // jmpF
        }
        break;
        
        case 6:
        {
          // push
        }
        break;
        
        case 7:
        {
          // invalid
        }
        break;
        
    }
  }
  break;

  case 0x0100:
  {
    switch(modrmcontext.greg)
    {
        case 0:
        {
          switch(modrmcontext.byte >> 6)
          {
            case 3:
            {
              // sldt
            }
            break;
            
            default:
            {
              // sldt
            }
            break;
            
          }
        }
        break;
        
        case 1:
        {
          switch(modrmcontext.byte >> 6)
          {
            case 3:
            {
              // str
            }
            break;
            
            default:
            {
              // str
            }
            break;
            
          }
        }
        break;
        
        case 2:
        {
          // lldt
        }
        break;
        
        case 3:
        {
          // ltr
        }
        break;
        
        case 4:
        {
          // verr
        }
        break;
        
        case 5:
        {
          // verw
        }
        break;
        
        case 6:
        {
          // invalid
        }
        break;
        
        case 7:
        {
          // invalid
        }
        break;
        
    }
  }
  break;

  case 0x0101:
  {
    switch(modrmcontext.byte >> 6)
    {
      case 3:
      {
        switch(modrmcontext.greg)
        {
            case 0:
            {
              switch(modrmcontext.greg)
              {
                  case 0:
                  {
                    // invalid
                  }
                  break;
                  
                  case 1:
                  {
                    // vmcall
                  }
                  break;
                  
                  case 2:
                  {
                    // vmlaunch
                  }
                  break;
                  
                  case 3:
                  {
                    // vmresume
                  }
                  break;
                  
                  case 4:
                  {
                    // vmxoff
                  }
                  break;
                  
                  case 5:
                  {
                    // invalid
                  }
                  break;
                  
                  case 6:
                  {
                    // invalid
                  }
                  break;
                  
                  case 7:
                  {
                    // invalid
                  }
                  break;
                  
              }
            }
            break;
            
            case 1:
            {
              switch(modrmcontext.greg)
              {
                  case 0:
                  {
                    // monitor
                  }
                  break;
                  
                  case 1:
                  {
                    // mwait
                  }
                  break;
                  
                  case 2:
                  {
                    // invalid
                  }
                  break;
                  
                  case 3:
                  {
                    // invalid
                  }
                  break;
                  
                  case 4:
                  {
                    // invalid
                  }
                  break;
                  
                  case 5:
                  {
                    // invalid
                  }
                  break;
                  
                  case 6:
                  {
                    // invalid
                  }
                  break;
                  
                  case 7:
                  {
                    // invalid
                  }
                  break;
                  
              }
            }
            break;
            
            case 2:
            {
              // invalid
            }
            break;
            
            case 3:
            {
              // invalid
            }
            break;
            
            case 4:
            {
              // smsw
            }
            break;
            
            case 5:
            {
              // invalid
            }
            break;
            
            case 6:
            {
              // lmsw
            }
            break;
            
            case 7:
            {
              // invalid
            }
            break;
            
        }
      }
      break;
      
      default:
      {
        switch(modrmcontext.greg)
        {
            case 0:
            {
              // sgdt
            }
            break;
            
            case 1:
            {
              // sidt
            }
            break;
            
            case 2:
            {
              // lgdt
            }
            break;
            
            case 3:
            {
              // lidt
            }
            break;
            
            case 4:
            {
              // smsw
            }
            break;
            
            case 5:
            {
              // invalid
            }
            break;
            
            case 6:
            {
              // lmsw
            }
            break;
            
            case 7:
            {
              // invlpg
            }
            break;
            
        }
      }
      break;
      
    }
  }
  break;

  case 0x0102:
  {
    // lar
  }
  break;

  case 0x0103:
  {
    // lsl
  }
  break;

  case 0x0104:
  {
    // invalid
  }
  break;

  case 0x0105:
  {
    // syscall
  }
  break;

  case 0x0106:
  {
    // clts
  }
  break;

  case 0x0107:
  {
    // sysret
  }
  break;

  case 0x0108:
  {
    // invd
  }
  break;

  case 0x0109:
  {
    // wbinvd
  }
  break;

  case 0x010a:
  {
    // invalid
  }
  break;

  case 0x010b:
  {
    // ud2
  }
  break;

  case 0x010c:
  {
    // invalid
  }
  break;

  case 0x010d:
  {
    // nop
  }
  break;

  case 0x010e:
  {
    // invalid
  }
  break;

  case 0x010f:
  {
    // invalid
  }
  break;

  case 0x0110:
  {
    // SSE/all
  }
  break;

  case 0x0111:
  {
    // SSE/all
  }
  break;

  case 0x0112:
  {
    // SSE/all
  }
  break;

  case 0x0113:
  {
    // SSE/no,66
  }
  break;

  case 0x0114:
  {
    // SSE/no,66
  }
  break;

  case 0x0115:
  {
    // SSE/no,66
  }
  break;

  case 0x0116:
  {
    // SSE/all
  }
  break;

  case 0x0117:
  {
    // SSE/no,66
  }
  break;

  case 0x0118:
  {
    switch(modrmcontext.byte >> 6)
    {
      case 3:
      {
        // invalid
      }
      break;
      
      default:
      {
        switch(modrmcontext.greg)
        {
            case 0:
            {
              // prefetchnta
            }
            break;
            
            case 1:
            {
              // prefetcht0
            }
            break;
            
            case 2:
            {
              // prefetcht1
            }
            break;
            
            case 3:
            {
              // prefetcht2
            }
            break;
            
            case 4:
            {
              // invalid
            }
            break;
            
            case 5:
            {
              // invalid
            }
            break;
            
            case 6:
            {
              // invalid
            }
            break;
            
            case 7:
            {
              // invalid
            }
            break;
            
        }
      }
      break;
      
    }
  }
  break;

  case 0x0119:
  {
    // invalid
  }
  break;

  case 0x011a:
  {
    // invalid
  }
  break;

  case 0x011b:
  {
    // invalid
  }
  break;

  case 0x011c:
  {
    // invalid
  }
  break;

  case 0x011d:
  {
    // invalid
  }
  break;

  case 0x011e:
  {
    // invalid
  }
  break;

  case 0x011f:
  {
    // nop
  }
  break;

  case 0x0120:
  {
    // mov
  }
  break;

  case 0x0121:
  {
    // mov
  }
  break;

  case 0x0122:
  {
    // mov
  }
  break;

  case 0x0123:
  {
    // mov
  }
  break;

  case 0x0124:
  {
    // invalid
  }
  break;

  case 0x0125:
  {
    // invalid
  }
  break;

  case 0x0126:
  {
    // invalid
  }
  break;

  case 0x0127:
  {
    // invalid
  }
  break;

  case 0x0128:
  {
    // SSE/no,66
  }
  break;

  case 0x0129:
  {
    // SSE/no,66
  }
  break;

  case 0x012a:
  {
    // SSE/all
  }
  break;

  case 0x012b:
  {
    // SSE/no,66
  }
  break;

  case 0x012c:
  {
    // SSE/all
  }
  break;

  case 0x012d:
  {
    // SSE/all
  }
  break;

  case 0x012e:
  {
    // SSE/no,66
  }
  break;

  case 0x012f:
  {
    // SSE/no,66
  }
  break;

  case 0x0130:
  {
    // wrmsr
  }
  break;

  case 0x0131:
  {
    // rdtsc
  }
  break;

  case 0x0132:
  {
    // rdmsr
  }
  break;

  case 0x0133:
  {
    // rdpmc
  }
  break;

  case 0x0134:
  {
    // sysenter
  }
  break;

  case 0x0135:
  {
    // sysexit
  }
  break;

  case 0x0136:
  {
    // invalid
  }
  break;

  case 0x0137:
  {
    // getsec
  }
  break;

  case 0x0138:
  {
    // fatal
  }
  break;

  case 0x0139:
  {
    // invalid
  }
  break;

  case 0x013a:
  {
    // fatal
  }
  break;

  case 0x013b:
  {
    // invalid
  }
  break;

  case 0x013c:
  {
    // invalid
  }
  break;

  case 0x013d:
  {
    // invalid
  }
  break;

  case 0x013e:
  {
    // invalid
  }
  break;

  case 0x013f:
  {
    // invalid
  }
  break;

  case 0x0140:
  {
    // cmovo
  }
  break;

  case 0x0141:
  {
    // cmovno
  }
  break;

  case 0x0142:
  {
    // cmovb
  }
  break;

  case 0x0143:
  {
    // cmovae
  }
  break;

  case 0x0144:
  {
    // cmovz
  }
  break;

  case 0x0145:
  {
    // cmovnz
  }
  break;

  case 0x0146:
  {
    // cmovbe
  }
  break;

  case 0x0147:
  {
    // cmova
  }
  break;

  case 0x0148:
  {
    // cmovs
  }
  break;

  case 0x0149:
  {
    // cmovns
  }
  break;

  case 0x014a:
  {
    // cmovp
  }
  break;

  case 0x014b:
  {
    // cmovnp
  }
  break;

  case 0x014c:
  {
    // cmovl
  }
  break;

  case 0x014d:
  {
    // cmovge
  }
  break;

  case 0x014e:
  {
    // cmovle
  }
  break;

  case 0x014f:
  {
    // cmovg
  }
  break;

  case 0x0150:
  {
    // SSE/no,66
  }
  break;

  case 0x0151:
  {
    // SSE/all
  }
  break;

  case 0x0152:
  {
    // SSE/all
  }
  break;

  case 0x0153:
  {
    // SSE/all
  }
  break;

  case 0x0154:
  {
    // SSE/no,66
  }
  break;

  case 0x0155:
  {
    // SSE/no,66
  }
  break;

  case 0x0156:
  {
    // SSE/no,66
  }
  break;

  case 0x0157:
  {
    // SSE/no,66
  }
  break;

  case 0x0158:
  {
    // SSE/all
  }
  break;

  case 0x0159:
  {
    // SSE/all
  }
  break;

  case 0x015a:
  {
    // SSE/all
  }
  break;

  case 0x015b:
  {
    // SSE/all
  }
  break;

  case 0x015c:
  {
    // SSE/all
  }
  break;

  case 0x015d:
  {
    // SSE/all
  }
  break;

  case 0x015e:
  {
    // SSE/all
  }
  break;

  case 0x015f:
  {
    // SSE/all
  }
  break;

  case 0x0160:
  {
    // SSE/no,66
  }
  break;

  case 0x0161:
  {
    // SSE/no,66
  }
  break;

  case 0x0162:
  {
    // SSE/no,66
  }
  break;

  case 0x0163:
  {
    // SSE/no,66
  }
  break;

  case 0x0164:
  {
    // SSE/no,66
  }
  break;

  case 0x0165:
  {
    // SSE/no,66
  }
  break;

  case 0x0166:
  {
    // SSE/no,66
  }
  break;

  case 0x0167:
  {
    // SSE/no,66
  }
  break;

  case 0x0168:
  {
    // SSE/no,66
  }
  break;

  case 0x0169:
  {
    // SSE/no,66
  }
  break;

  case 0x016a:
  {
    // SSE/no,66
  }
  break;

  case 0x016b:
  {
    // SSE/no,66
  }
  break;

  case 0x016c:
  {
    // SSE/66
  }
  break;

  case 0x016d:
  {
    // SSE/66
  }
  break;

  case 0x016e:
  {
    // SSE/no,66
  }
  break;

  case 0x016f:
  {
    // SSE/all
  }
  break;

  case 0x0170:
  {
    // SSE/all
  }
  break;

  case 0x0171:
  {
    switch(modrmcontext.byte >> 6)
    {
      case 3:
      {
        switch(modrmcontext.greg)
        {
            case 0:
            {
              // invalid
            }
            break;
            
            case 1:
            {
              // invalid
            }
            break;
            
            case 2:
            {
              // SSE/no,66
            }
            break;
            
            case 3:
            {
              // invalid
            }
            break;
            
            case 4:
            {
              // SSE/no,66
            }
            break;
            
            case 5:
            {
              // invalid
            }
            break;
            
            case 6:
            {
              // SSE/no,66
            }
            break;
            
            case 7:
            {
              // invalid
            }
            break;
            
        }
      }
      break;
      
      default:
      {
        // invalid
      }
      break;
      
    }
  }
  break;

  case 0x0172:
  {
    switch(modrmcontext.byte >> 6)
    {
      case 3:
      {
        switch(modrmcontext.greg)
        {
            case 0:
            {
              // invalid
            }
            break;
            
            case 1:
            {
              // invalid
            }
            break;
            
            case 2:
            {
              // SSE/no,66
            }
            break;
            
            case 3:
            {
              // invalid
            }
            break;
            
            case 4:
            {
              // SSE/no,66
            }
            break;
            
            case 5:
            {
              // invalid
            }
            break;
            
            case 6:
            {
              // SSE/no,66
            }
            break;
            
            case 7:
            {
              // invalid
            }
            break;
            
        }
      }
      break;
      
      default:
      {
        // invalid
      }
      break;
      
    }
  }
  break;

  case 0x0173:
  {
    switch(modrmcontext.byte >> 6)
    {
      case 3:
      {
        switch(modrmcontext.greg)
        {
            case 0:
            {
              // invalid
            }
            break;
            
            case 1:
            {
              // invalid
            }
            break;
            
            case 2:
            {
              // SSE/no,66
            }
            break;
            
            case 3:
            {
              // SSE/66
            }
            break;
            
            case 4:
            {
              // invalid
            }
            break;
            
            case 5:
            {
              // invalid
            }
            break;
            
            case 6:
            {
              // SSE/no,66
            }
            break;
            
            case 7:
            {
              // SSE/66
            }
            break;
            
        }
      }
      break;
      
      default:
      {
        // invalid
      }
      break;
      
    }
  }
  break;

  case 0x0174:
  {
    // SSE/no,66
  }
  break;

  case 0x0175:
  {
    // SSE/no,66
  }
  break;

  case 0x0176:
  {
    // SSE/no,66
  }
  break;

  case 0x0177:
  {
    // emms
  }
  break;

  case 0x0178:
  {
    // vmread
  }
  break;

  case 0x0179:
  {
    // vmwrite
  }
  break;

  case 0x017a:
  {
    // invalid
  }
  break;

  case 0x017b:
  {
    // invalid
  }
  break;

  case 0x017c:
  {
    // SSE/all
  }
  break;

  case 0x017d:
  {
    // SSE/all
  }
  break;

  case 0x017e:
  {
    // SSE/all
  }
  break;

  case 0x017f:
  {
    // SSE/all
  }
  break;

  case 0x0180:
  {
    // jo
  }
  break;

  case 0x0181:
  {
    // jno
  }
  break;

  case 0x0182:
  {
    // jb
  }
  break;

  case 0x0183:
  {
    // jae
  }
  break;

  case 0x0184:
  {
    // jz
  }
  break;

  case 0x0185:
  {
    // jnz
  }
  break;

  case 0x0186:
  {
    // jbe
  }
  break;

  case 0x0187:
  {
    // ja
  }
  break;

  case 0x0188:
  {
    // js
  }
  break;

  case 0x0189:
  {
    // jns
  }
  break;

  case 0x018a:
  {
    // jp
  }
  break;

  case 0x018b:
  {
    // jnp
  }
  break;

  case 0x018c:
  {
    // jl
  }
  break;

  case 0x018d:
  {
    // jge
  }
  break;

  case 0x018e:
  {
    // jle
  }
  break;

  case 0x018f:
  {
    // jg
  }
  break;

  case 0x0190:
  {
    // seto
  }
  break;

  case 0x0191:
  {
    // setno
  }
  break;

  case 0x0192:
  {
    // setb
  }
  break;

  case 0x0193:
  {
    // setae
  }
  break;

  case 0x0194:
  {
    // setz
  }
  break;

  case 0x0195:
  {
    // setnz
  }
  break;

  case 0x0196:
  {
    // setbe
  }
  break;

  case 0x0197:
  {
    // seta
  }
  break;

  case 0x0198:
  {
    // sets
  }
  break;

  case 0x0199:
  {
    // setns
  }
  break;

  case 0x019a:
  {
    // setp
  }
  break;

  case 0x019b:
  {
    // setnp
  }
  break;

  case 0x019c:
  {
    // setl
  }
  break;

  case 0x019d:
  {
    // setge
  }
  break;

  case 0x019e:
  {
    // setle
  }
  break;

  case 0x019f:
  {
    // setg
  }
  break;

  case 0x01a0:
  {
    // push
  }
  break;

  case 0x01a1:
  {
    // pop
  }
  break;

  case 0x01a2:
  {
    // cpuid
  }
  break;

  case 0x01a3:
  {
    // bt
  }
  break;

  case 0x01a4:
  {
    // shld
  }
  break;

  case 0x01a5:
  {
    // shld
  }
  break;

  case 0x01a6:
  {
    // invalid
  }
  break;

  case 0x01a7:
  {
    // invalid
  }
  break;

  case 0x01a8:
  {
    // push
  }
  break;

  case 0x01a9:
  {
    // pop
  }
  break;

  case 0x01aa:
  {
    // rsm
  }
  break;

  case 0x01ab:
  {
    // bts
  }
  break;

  case 0x01ac:
  {
    // shrd
  }
  break;

  case 0x01ad:
  {
    // shrd
  }
  break;

  case 0x01ae:
  {
    switch(modrmcontext.byte >> 6)
    {
      case 3:
      {
        switch(modrmcontext.greg)
        {
            case 0:
            {
              // invalid
            }
            break;
            
            case 1:
            {
              // invalid
            }
            break;
            
            case 2:
            {
              // invalid
            }
            break;
            
            case 3:
            {
              // invalid
            }
            break;
            
            case 4:
            {
              // invalid
            }
            break;
            
            case 5:
            {
              // lfence
            }
            break;
            
            case 6:
            {
              // mfence
            }
            break;
            
            case 7:
            {
              // sfence
            }
            break;
            
        }
      }
      break;
      
      default:
      {
        switch(modrmcontext.greg)
        {
            case 0:
            {
              // fxsave
            }
            break;
            
            case 1:
            {
              // fxrstor
            }
            break;
            
            case 2:
            {
              // ldmxcsr
            }
            break;
            
            case 3:
            {
              // stmxcsr
            }
            break;
            
            case 4:
            {
              // invalid
            }
            break;
            
            case 5:
            {
              // invalid
            }
            break;
            
            case 6:
            {
              // invalid
            }
            break;
            
            case 7:
            {
              // clflush
            }
            break;
            
        }
      }
      break;
      
    }
  }
  break;

  case 0x01af:
  {
    // imul
  }
  break;

  case 0x01b0:
  {
    // cmpxchg
  }
  break;

  case 0x01b1:
  {
    // cmpxchg
  }
  break;

  case 0x01b2:
  {
    // lss
  }
  break;

  case 0x01b3:
  {
    // btr
  }
  break;

  case 0x01b4:
  {
    // lfs
  }
  break;

  case 0x01b5:
  {
    // lgs
  }
  break;

  case 0x01b6:
  {
    // movzx
  }
  break;

  case 0x01b7:
  {
    // movzx
  }
  break;

  case 0x01b8:
  {
    // popcnt
  }
  break;

  case 0x01b9:
  {
    // invalid
  }
  break;

  case 0x01ba:
  {
    switch(modrmcontext.greg)
    {
        case 0:
        {
          // invalid
        }
        break;
        
        case 1:
        {
          // invalid
        }
        break;
        
        case 2:
        {
          // invalid
        }
        break;
        
        case 3:
        {
          // invalid
        }
        break;
        
        case 4:
        {
          // bt
        }
        break;
        
        case 5:
        {
          // bts
        }
        break;
        
        case 6:
        {
          // btr
        }
        break;
        
        case 7:
        {
          // btc
        }
        break;
        
    }
  }
  break;

  case 0x01bb:
  {
    // btc
  }
  break;

  case 0x01bc:
  {
    // bsf
  }
  break;

  case 0x01bd:
  {
    // bsr
  }
  break;

  case 0x01be:
  {
    // movsx
  }
  break;

  case 0x01bf:
  {
    // movsx
  }
  break;

  case 0x01c0:
  {
    // xadd
  }
  break;

  case 0x01c1:
  {
    // xadd
  }
  break;

  case 0x01c2:
  {
    // SSE/all
  }
  break;

  case 0x01c3:
  {
    // SSE/no
  }
  break;

  case 0x01c4:
  {
    // SSE/no,66
  }
  break;

  case 0x01c5:
  {
    // SSE/no,66
  }
  break;

  case 0x01c6:
  {
    // SSE/no,66
  }
  break;

  case 0x01c7:
  {
    switch(modrmcontext.byte >> 6)
    {
      case 3:
      {
        // invalid
      }
      break;
      
      default:
      {
        switch(modrmcontext.greg)
        {
            case 0:
            {
              // invalid
            }
            break;
            
            case 1:
            {
              // cmpxchg8b
            }
            break;
            
            case 2:
            {
              // invalid
            }
            break;
            
            case 3:
            {
              // invalid
            }
            break;
            
            case 4:
            {
              // invalid
            }
            break;
            
            case 5:
            {
              // invalid
            }
            break;
            
            case 6:
            {
              // SSE/all
            }
            break;
            
            case 7:
            {
              // SSE/no
            }
            break;
            
        }
      }
      break;
      
    }
  }
  break;

  case 0x01c8:
  {
    // bswap
  }
  break;

  case 0x01c9:
  {
    // bswap
  }
  break;

  case 0x01ca:
  {
    // bswap
  }
  break;

  case 0x01cb:
  {
    // bswap
  }
  break;

  case 0x01cc:
  {
    // bswap
  }
  break;

  case 0x01cd:
  {
    // bswap
  }
  break;

  case 0x01ce:
  {
    // bswap
  }
  break;

  case 0x01cf:
  {
    // bswap
  }
  break;

  case 0x01d0:
  {
    // SSE/all
  }
  break;

  case 0x01d1:
  {
    // SSE/no,66
  }
  break;

  case 0x01d2:
  {
    // SSE/no,66
  }
  break;

  case 0x01d3:
  {
    // SSE/no,66
  }
  break;

  case 0x01d4:
  {
    // SSE/no,66
  }
  break;

  case 0x01d5:
  {
    // SSE/no,66
  }
  break;

  case 0x01d6:
  {
    // SSE/all
  }
  break;

  case 0x01d7:
  {
    // SSE/no,66
  }
  break;

  case 0x01d8:
  {
    // SSE/no,66
  }
  break;

  case 0x01d9:
  {
    // SSE/no,66
  }
  break;

  case 0x01da:
  {
    // SSE/no,66
  }
  break;

  case 0x01db:
  {
    // SSE/no,66
  }
  break;

  case 0x01dc:
  {
    // SSE/no,66
  }
  break;

  case 0x01dd:
  {
    // SSE/no,66
  }
  break;

  case 0x01de:
  {
    // SSE/no,66
  }
  break;

  case 0x01df:
  {
    // SSE/no,66
  }
  break;

  case 0x01e0:
  {
    // SSE/no,66
  }
  break;

  case 0x01e1:
  {
    // SSE/no,66
  }
  break;

  case 0x01e2:
  {
    // SSE/no,66
  }
  break;

  case 0x01e3:
  {
    // SSE/no,66
  }
  break;

  case 0x01e4:
  {
    // SSE/no,66
  }
  break;

  case 0x01e5:
  {
    // SSE/no,66
  }
  break;

  case 0x01e6:
  {
    // SSE/all
  }
  break;

  case 0x01e7:
  {
    // SSE/no,66
  }
  break;

  case 0x01e8:
  {
    // SSE/no,66
  }
  break;

  case 0x01e9:
  {
    // SSE/no,66
  }
  break;

  case 0x01ea:
  {
    // SSE/no,66
  }
  break;

  case 0x01eb:
  {
    // SSE/no,66
  }
  break;

  case 0x01ec:
  {
    // SSE/no,66
  }
  break;

  case 0x01ed:
  {
    // SSE/no,66
  }
  break;

  case 0x01ee:
  {
    // SSE/no,66
  }
  break;

  case 0x01ef:
  {
    // SSE/no,66
  }
  break;

  case 0x01f0:
  {
    // SSE/all
  }
  break;

  case 0x01f1:
  {
    // SSE/no,66
  }
  break;

  case 0x01f2:
  {
    // SSE/no,66
  }
  break;

  case 0x01f3:
  {
    // SSE/no,66
  }
  break;

  case 0x01f4:
  {
    // SSE/no,66
  }
  break;

  case 0x01f5:
  {
    // SSE/no,66
  }
  break;

  case 0x01f6:
  {
    // SSE/no,66
  }
  break;

  case 0x01f7:
  {
    // SSE/no,66
  }
  break;

  case 0x01f8:
  {
    // SSE/no,66
  }
  break;

  case 0x01f9:
  {
    // SSE/no,66
  }
  break;

  case 0x01fa:
  {
    // SSE/no,66
  }
  break;

  case 0x01fb:
  {
    // SSE/no,66
  }
  break;

  case 0x01fc:
  {
    // SSE/no,66
  }
  break;

  case 0x01fd:
  {
    // SSE/no,66
  }
  break;

  case 0x01fe:
  {
    // SSE/no,66
  }
  break;

  case 0x01ff:
  {
    // invalid
  }
  break;

  case 0x0200:
  {
    // SSE/no,66
  }
  break;

  case 0x0201:
  {
    // SSE/no,66
  }
  break;

  case 0x0202:
  {
    // SSE/no,66
  }
  break;

  case 0x0203:
  {
    // SSE/no,66
  }
  break;

  case 0x0204:
  {
    // SSE/no,66
  }
  break;

  case 0x0205:
  {
    // SSE/no,66
  }
  break;

  case 0x0206:
  {
    // SSE/no,66
  }
  break;

  case 0x0207:
  {
    // SSE/no,66
  }
  break;

  case 0x0208:
  {
    // SSE/no,66
  }
  break;

  case 0x0209:
  {
    // SSE/no,66
  }
  break;

  case 0x020a:
  {
    // SSE/no,66
  }
  break;

  case 0x020b:
  {
    // SSE/no,66
  }
  break;

  case 0x020c:
  {
    // invalid
  }
  break;

  case 0x020d:
  {
    // invalid
  }
  break;

  case 0x020e:
  {
    // invalid
  }
  break;

  case 0x020f:
  {
    // invalid
  }
  break;

  case 0x0210:
  {
    // SSE/66
  }
  break;

  case 0x0211:
  {
    // invalid
  }
  break;

  case 0x0212:
  {
    // invalid
  }
  break;

  case 0x0213:
  {
    // invalid
  }
  break;

  case 0x0214:
  {
    // SSE/66
  }
  break;

  case 0x0215:
  {
    // SSE/66
  }
  break;

  case 0x0216:
  {
    // invalid
  }
  break;

  case 0x0217:
  {
    // SSE/66
  }
  break;

  case 0x0218:
  {
    // invalid
  }
  break;

  case 0x0219:
  {
    // invalid
  }
  break;

  case 0x021a:
  {
    // invalid
  }
  break;

  case 0x021b:
  {
    // invalid
  }
  break;

  case 0x021c:
  {
    // SSE/no,66
  }
  break;

  case 0x021d:
  {
    // SSE/no,66
  }
  break;

  case 0x021e:
  {
    // SSE/no,66
  }
  break;

  case 0x021f:
  {
    // invalid
  }
  break;

  case 0x0220:
  {
    // SSE/66
  }
  break;

  case 0x0221:
  {
    // SSE/66
  }
  break;

  case 0x0222:
  {
    // SSE/66
  }
  break;

  case 0x0223:
  {
    // SSE/66
  }
  break;

  case 0x0224:
  {
    // SSE/66
  }
  break;

  case 0x0225:
  {
    // SSE/66
  }
  break;

  case 0x0226:
  {
    // invalid
  }
  break;

  case 0x0227:
  {
    // invalid
  }
  break;

  case 0x0228:
  {
    // SSE/66
  }
  break;

  case 0x0229:
  {
    // SSE/66
  }
  break;

  case 0x022a:
  {
    // SSE/66
  }
  break;

  case 0x022b:
  {
    // SSE/66
  }
  break;

  case 0x022c:
  {
    // invalid
  }
  break;

  case 0x022d:
  {
    // invalid
  }
  break;

  case 0x022e:
  {
    // invalid
  }
  break;

  case 0x022f:
  {
    // invalid
  }
  break;

  case 0x0230:
  {
    // SSE/66
  }
  break;

  case 0x0231:
  {
    // SSE/66
  }
  break;

  case 0x0232:
  {
    // SSE/66
  }
  break;

  case 0x0233:
  {
    // SSE/66
  }
  break;

  case 0x0234:
  {
    // SSE/66
  }
  break;

  case 0x0235:
  {
    // SSE/66
  }
  break;

  case 0x0236:
  {
    // invalid
  }
  break;

  case 0x0237:
  {
    // SSE/66
  }
  break;

  case 0x0238:
  {
    // SSE/66
  }
  break;

  case 0x0239:
  {
    // SSE/66
  }
  break;

  case 0x023a:
  {
    // SSE/66
  }
  break;

  case 0x023b:
  {
    // SSE/66
  }
  break;

  case 0x023c:
  {
    // SSE/66
  }
  break;

  case 0x023d:
  {
    // SSE/66
  }
  break;

  case 0x023e:
  {
    // SSE/66
  }
  break;

  case 0x023f:
  {
    // SSE/66
  }
  break;

  case 0x0240:
  {
    // SSE/66
  }
  break;

  case 0x0241:
  {
    // SSE/66
  }
  break;

  case 0x0242:
  {
    // invalid
  }
  break;

  case 0x0243:
  {
    // invalid
  }
  break;

  case 0x0244:
  {
    // invalid
  }
  break;

  case 0x0245:
  {
    // invalid
  }
  break;

  case 0x0246:
  {
    // invalid
  }
  break;

  case 0x0247:
  {
    // invalid
  }
  break;

  case 0x0248:
  {
    // invalid
  }
  break;

  case 0x0249:
  {
    // invalid
  }
  break;

  case 0x024a:
  {
    // invalid
  }
  break;

  case 0x024b:
  {
    // invalid
  }
  break;

  case 0x024c:
  {
    // invalid
  }
  break;

  case 0x024d:
  {
    // invalid
  }
  break;

  case 0x024e:
  {
    // invalid
  }
  break;

  case 0x024f:
  {
    // invalid
  }
  break;

  case 0x0250:
  {
    // invalid
  }
  break;

  case 0x0251:
  {
    // invalid
  }
  break;

  case 0x0252:
  {
    // invalid
  }
  break;

  case 0x0253:
  {
    // invalid
  }
  break;

  case 0x0254:
  {
    // invalid
  }
  break;

  case 0x0255:
  {
    // invalid
  }
  break;

  case 0x0256:
  {
    // invalid
  }
  break;

  case 0x0257:
  {
    // invalid
  }
  break;

  case 0x0258:
  {
    // invalid
  }
  break;

  case 0x0259:
  {
    // invalid
  }
  break;

  case 0x025a:
  {
    // invalid
  }
  break;

  case 0x025b:
  {
    // invalid
  }
  break;

  case 0x025c:
  {
    // invalid
  }
  break;

  case 0x025d:
  {
    // invalid
  }
  break;

  case 0x025e:
  {
    // invalid
  }
  break;

  case 0x025f:
  {
    // invalid
  }
  break;

  case 0x0260:
  {
    // invalid
  }
  break;

  case 0x0261:
  {
    // invalid
  }
  break;

  case 0x0262:
  {
    // invalid
  }
  break;

  case 0x0263:
  {
    // invalid
  }
  break;

  case 0x0264:
  {
    // invalid
  }
  break;

  case 0x0265:
  {
    // invalid
  }
  break;

  case 0x0266:
  {
    // invalid
  }
  break;

  case 0x0267:
  {
    // invalid
  }
  break;

  case 0x0268:
  {
    // invalid
  }
  break;

  case 0x0269:
  {
    // invalid
  }
  break;

  case 0x026a:
  {
    // invalid
  }
  break;

  case 0x026b:
  {
    // invalid
  }
  break;

  case 0x026c:
  {
    // invalid
  }
  break;

  case 0x026d:
  {
    // invalid
  }
  break;

  case 0x026e:
  {
    // invalid
  }
  break;

  case 0x026f:
  {
    // invalid
  }
  break;

  case 0x0270:
  {
    // invalid
  }
  break;

  case 0x0271:
  {
    // invalid
  }
  break;

  case 0x0272:
  {
    // invalid
  }
  break;

  case 0x0273:
  {
    // invalid
  }
  break;

  case 0x0274:
  {
    // invalid
  }
  break;

  case 0x0275:
  {
    // invalid
  }
  break;

  case 0x0276:
  {
    // invalid
  }
  break;

  case 0x0277:
  {
    // invalid
  }
  break;

  case 0x0278:
  {
    // invalid
  }
  break;

  case 0x0279:
  {
    // invalid
  }
  break;

  case 0x027a:
  {
    // invalid
  }
  break;

  case 0x027b:
  {
    // invalid
  }
  break;

  case 0x027c:
  {
    // invalid
  }
  break;

  case 0x027d:
  {
    // invalid
  }
  break;

  case 0x027e:
  {
    // invalid
  }
  break;

  case 0x027f:
  {
    // invalid
  }
  break;

  case 0x0280:
  {
    // invalid
  }
  break;

  case 0x0281:
  {
    // invalid
  }
  break;

  case 0x0282:
  {
    // invalid
  }
  break;

  case 0x0283:
  {
    // invalid
  }
  break;

  case 0x0284:
  {
    // invalid
  }
  break;

  case 0x0285:
  {
    // invalid
  }
  break;

  case 0x0286:
  {
    // invalid
  }
  break;

  case 0x0287:
  {
    // invalid
  }
  break;

  case 0x0288:
  {
    // invalid
  }
  break;

  case 0x0289:
  {
    // invalid
  }
  break;

  case 0x028a:
  {
    // invalid
  }
  break;

  case 0x028b:
  {
    // invalid
  }
  break;

  case 0x028c:
  {
    // invalid
  }
  break;

  case 0x028d:
  {
    // invalid
  }
  break;

  case 0x028e:
  {
    // invalid
  }
  break;

  case 0x028f:
  {
    // invalid
  }
  break;

  case 0x0290:
  {
    // invalid
  }
  break;

  case 0x0291:
  {
    // invalid
  }
  break;

  case 0x0292:
  {
    // invalid
  }
  break;

  case 0x0293:
  {
    // invalid
  }
  break;

  case 0x0294:
  {
    // invalid
  }
  break;

  case 0x0295:
  {
    // invalid
  }
  break;

  case 0x0296:
  {
    // invalid
  }
  break;

  case 0x0297:
  {
    // invalid
  }
  break;

  case 0x0298:
  {
    // invalid
  }
  break;

  case 0x0299:
  {
    // invalid
  }
  break;

  case 0x029a:
  {
    // invalid
  }
  break;

  case 0x029b:
  {
    // invalid
  }
  break;

  case 0x029c:
  {
    // invalid
  }
  break;

  case 0x029d:
  {
    // invalid
  }
  break;

  case 0x029e:
  {
    // invalid
  }
  break;

  case 0x029f:
  {
    // invalid
  }
  break;

  case 0x02a0:
  {
    // invalid
  }
  break;

  case 0x02a1:
  {
    // invalid
  }
  break;

  case 0x02a2:
  {
    // invalid
  }
  break;

  case 0x02a3:
  {
    // invalid
  }
  break;

  case 0x02a4:
  {
    // invalid
  }
  break;

  case 0x02a5:
  {
    // invalid
  }
  break;

  case 0x02a6:
  {
    // invalid
  }
  break;

  case 0x02a7:
  {
    // invalid
  }
  break;

  case 0x02a8:
  {
    // invalid
  }
  break;

  case 0x02a9:
  {
    // invalid
  }
  break;

  case 0x02aa:
  {
    // invalid
  }
  break;

  case 0x02ab:
  {
    // invalid
  }
  break;

  case 0x02ac:
  {
    // invalid
  }
  break;

  case 0x02ad:
  {
    // invalid
  }
  break;

  case 0x02ae:
  {
    // invalid
  }
  break;

  case 0x02af:
  {
    // invalid
  }
  break;

  case 0x02b0:
  {
    // invalid
  }
  break;

  case 0x02b1:
  {
    // invalid
  }
  break;

  case 0x02b2:
  {
    // invalid
  }
  break;

  case 0x02b3:
  {
    // invalid
  }
  break;

  case 0x02b4:
  {
    // invalid
  }
  break;

  case 0x02b5:
  {
    // invalid
  }
  break;

  case 0x02b6:
  {
    // invalid
  }
  break;

  case 0x02b7:
  {
    // invalid
  }
  break;

  case 0x02b8:
  {
    // invalid
  }
  break;

  case 0x02b9:
  {
    // invalid
  }
  break;

  case 0x02ba:
  {
    // invalid
  }
  break;

  case 0x02bb:
  {
    // invalid
  }
  break;

  case 0x02bc:
  {
    // invalid
  }
  break;

  case 0x02bd:
  {
    // invalid
  }
  break;

  case 0x02be:
  {
    // invalid
  }
  break;

  case 0x02bf:
  {
    // invalid
  }
  break;

  case 0x02c0:
  {
    // invalid
  }
  break;

  case 0x02c1:
  {
    // invalid
  }
  break;

  case 0x02c2:
  {
    // invalid
  }
  break;

  case 0x02c3:
  {
    // invalid
  }
  break;

  case 0x02c4:
  {
    // invalid
  }
  break;

  case 0x02c5:
  {
    // invalid
  }
  break;

  case 0x02c6:
  {
    // invalid
  }
  break;

  case 0x02c7:
  {
    // invalid
  }
  break;

  case 0x02c8:
  {
    // invalid
  }
  break;

  case 0x02c9:
  {
    // invalid
  }
  break;

  case 0x02ca:
  {
    // invalid
  }
  break;

  case 0x02cb:
  {
    // invalid
  }
  break;

  case 0x02cc:
  {
    // invalid
  }
  break;

  case 0x02cd:
  {
    // invalid
  }
  break;

  case 0x02ce:
  {
    // invalid
  }
  break;

  case 0x02cf:
  {
    // invalid
  }
  break;

  case 0x02d0:
  {
    // invalid
  }
  break;

  case 0x02d1:
  {
    // invalid
  }
  break;

  case 0x02d2:
  {
    // invalid
  }
  break;

  case 0x02d3:
  {
    // invalid
  }
  break;

  case 0x02d4:
  {
    // invalid
  }
  break;

  case 0x02d5:
  {
    // invalid
  }
  break;

  case 0x02d6:
  {
    // invalid
  }
  break;

  case 0x02d7:
  {
    // invalid
  }
  break;

  case 0x02d8:
  {
    // invalid
  }
  break;

  case 0x02d9:
  {
    // invalid
  }
  break;

  case 0x02da:
  {
    // invalid
  }
  break;

  case 0x02db:
  {
    // invalid
  }
  break;

  case 0x02dc:
  {
    // invalid
  }
  break;

  case 0x02dd:
  {
    // invalid
  }
  break;

  case 0x02de:
  {
    // invalid
  }
  break;

  case 0x02df:
  {
    // invalid
  }
  break;

  case 0x02e0:
  {
    // invalid
  }
  break;

  case 0x02e1:
  {
    // invalid
  }
  break;

  case 0x02e2:
  {
    // invalid
  }
  break;

  case 0x02e3:
  {
    // invalid
  }
  break;

  case 0x02e4:
  {
    // invalid
  }
  break;

  case 0x02e5:
  {
    // invalid
  }
  break;

  case 0x02e6:
  {
    // invalid
  }
  break;

  case 0x02e7:
  {
    // invalid
  }
  break;

  case 0x02e8:
  {
    // invalid
  }
  break;

  case 0x02e9:
  {
    // invalid
  }
  break;

  case 0x02ea:
  {
    // invalid
  }
  break;

  case 0x02eb:
  {
    // invalid
  }
  break;

  case 0x02ec:
  {
    // invalid
  }
  break;

  case 0x02ed:
  {
    // invalid
  }
  break;

  case 0x02ee:
  {
    // invalid
  }
  break;

  case 0x02ef:
  {
    // invalid
  }
  break;

  case 0x02f0:
  {
    // crc32
  }
  break;

  case 0x02f1:
  {
    // crc32
  }
  break;

  case 0x02f2:
  {
    // invalid
  }
  break;

  case 0x02f3:
  {
    // invalid
  }
  break;

  case 0x02f4:
  {
    // invalid
  }
  break;

  case 0x02f5:
  {
    // invalid
  }
  break;

  case 0x02f6:
  {
    // invalid
  }
  break;

  case 0x02f7:
  {
    // invalid
  }
  break;

  case 0x02f8:
  {
    // invalid
  }
  break;

  case 0x02f9:
  {
    // invalid
  }
  break;

  case 0x02fa:
  {
    // invalid
  }
  break;

  case 0x02fb:
  {
    // invalid
  }
  break;

  case 0x02fc:
  {
    // invalid
  }
  break;

  case 0x02fd:
  {
    // invalid
  }
  break;

  case 0x02fe:
  {
    // invalid
  }
  break;

  case 0x02ff:
  {
    // invalid
  }
  break;

  case 0x0300:
  {
    // invalid
  }
  break;

  case 0x0301:
  {
    // invalid
  }
  break;

  case 0x0302:
  {
    // invalid
  }
  break;

  case 0x0303:
  {
    // invalid
  }
  break;

  case 0x0304:
  {
    // invalid
  }
  break;

  case 0x0305:
  {
    // invalid
  }
  break;

  case 0x0306:
  {
    // invalid
  }
  break;

  case 0x0307:
  {
    // invalid
  }
  break;

  case 0x0308:
  {
    // SSE/66
  }
  break;

  case 0x0309:
  {
    // SSE/66
  }
  break;

  case 0x030a:
  {
    // SSE/66
  }
  break;

  case 0x030b:
  {
    // SSE/66
  }
  break;

  case 0x030c:
  {
    // SSE/66
  }
  break;

  case 0x030d:
  {
    // SSE/66
  }
  break;

  case 0x030e:
  {
    // SSE/66
  }
  break;

  case 0x030f:
  {
    // SSE/no,66
  }
  break;

  case 0x0310:
  {
    // invalid
  }
  break;

  case 0x0311:
  {
    // invalid
  }
  break;

  case 0x0312:
  {
    // invalid
  }
  break;

  case 0x0313:
  {
    // invalid
  }
  break;

  case 0x0314:
  {
    // SSE/66
  }
  break;

  case 0x0315:
  {
    // SSE/66
  }
  break;

  case 0x0316:
  {
    // SSE/66
  }
  break;

  case 0x0317:
  {
    // SSE/66
  }
  break;

  case 0x0318:
  {
    // invalid
  }
  break;

  case 0x0319:
  {
    // invalid
  }
  break;

  case 0x031a:
  {
    // invalid
  }
  break;

  case 0x031b:
  {
    // invalid
  }
  break;

  case 0x031c:
  {
    // invalid
  }
  break;

  case 0x031d:
  {
    // invalid
  }
  break;

  case 0x031e:
  {
    // invalid
  }
  break;

  case 0x031f:
  {
    // invalid
  }
  break;

  case 0x0320:
  {
    // SSE/66
  }
  break;

  case 0x0321:
  {
    // SSE/66
  }
  break;

  case 0x0322:
  {
    // SSE/66
  }
  break;

  case 0x0323:
  {
    // invalid
  }
  break;

  case 0x0324:
  {
    // invalid
  }
  break;

  case 0x0325:
  {
    // invalid
  }
  break;

  case 0x0326:
  {
    // invalid
  }
  break;

  case 0x0327:
  {
    // invalid
  }
  break;

  case 0x0328:
  {
    // invalid
  }
  break;

  case 0x0329:
  {
    // invalid
  }
  break;

  case 0x032a:
  {
    // invalid
  }
  break;

  case 0x032b:
  {
    // invalid
  }
  break;

  case 0x032c:
  {
    // invalid
  }
  break;

  case 0x032d:
  {
    // invalid
  }
  break;

  case 0x032e:
  {
    // invalid
  }
  break;

  case 0x032f:
  {
    // invalid
  }
  break;

  case 0x0330:
  {
    // invalid
  }
  break;

  case 0x0331:
  {
    // invalid
  }
  break;

  case 0x0332:
  {
    // invalid
  }
  break;

  case 0x0333:
  {
    // invalid
  }
  break;

  case 0x0334:
  {
    // invalid
  }
  break;

  case 0x0335:
  {
    // invalid
  }
  break;

  case 0x0336:
  {
    // invalid
  }
  break;

  case 0x0337:
  {
    // invalid
  }
  break;

  case 0x0338:
  {
    // invalid
  }
  break;

  case 0x0339:
  {
    // invalid
  }
  break;

  case 0x033a:
  {
    // invalid
  }
  break;

  case 0x033b:
  {
    // invalid
  }
  break;

  case 0x033c:
  {
    // invalid
  }
  break;

  case 0x033d:
  {
    // invalid
  }
  break;

  case 0x033e:
  {
    // invalid
  }
  break;

  case 0x033f:
  {
    // invalid
  }
  break;

  case 0x0340:
  {
    // SSE/66
  }
  break;

  case 0x0341:
  {
    // SSE/66
  }
  break;

  case 0x0342:
  {
    // SSE/66
  }
  break;

  case 0x0343:
  {
    // invalid
  }
  break;

  case 0x0344:
  {
    // invalid
  }
  break;

  case 0x0345:
  {
    // invalid
  }
  break;

  case 0x0346:
  {
    // invalid
  }
  break;

  case 0x0347:
  {
    // invalid
  }
  break;

  case 0x0348:
  {
    // invalid
  }
  break;

  case 0x0349:
  {
    // invalid
  }
  break;

  case 0x034a:
  {
    // invalid
  }
  break;

  case 0x034b:
  {
    // invalid
  }
  break;

  case 0x034c:
  {
    // invalid
  }
  break;

  case 0x034d:
  {
    // invalid
  }
  break;

  case 0x034e:
  {
    // invalid
  }
  break;

  case 0x034f:
  {
    // invalid
  }
  break;

  case 0x0350:
  {
    // invalid
  }
  break;

  case 0x0351:
  {
    // invalid
  }
  break;

  case 0x0352:
  {
    // invalid
  }
  break;

  case 0x0353:
  {
    // invalid
  }
  break;

  case 0x0354:
  {
    // invalid
  }
  break;

  case 0x0355:
  {
    // invalid
  }
  break;

  case 0x0356:
  {
    // invalid
  }
  break;

  case 0x0357:
  {
    // invalid
  }
  break;

  case 0x0358:
  {
    // invalid
  }
  break;

  case 0x0359:
  {
    // invalid
  }
  break;

  case 0x035a:
  {
    // invalid
  }
  break;

  case 0x035b:
  {
    // invalid
  }
  break;

  case 0x035c:
  {
    // invalid
  }
  break;

  case 0x035d:
  {
    // invalid
  }
  break;

  case 0x035e:
  {
    // invalid
  }
  break;

  case 0x035f:
  {
    // invalid
  }
  break;

  case 0x0360:
  {
    // SSE/66
  }
  break;

  case 0x0361:
  {
    // SSE/66
  }
  break;

  case 0x0362:
  {
    // SSE/66
  }
  break;

  case 0x0363:
  {
    // SSE/66
  }
  break;

  case 0x0364:
  {
    // invalid
  }
  break;

  case 0x0365:
  {
    // invalid
  }
  break;

  case 0x0366:
  {
    // invalid
  }
  break;

  case 0x0367:
  {
    // invalid
  }
  break;

  case 0x0368:
  {
    // invalid
  }
  break;

  case 0x0369:
  {
    // invalid
  }
  break;

  case 0x036a:
  {
    // invalid
  }
  break;

  case 0x036b:
  {
    // invalid
  }
  break;

  case 0x036c:
  {
    // invalid
  }
  break;

  case 0x036d:
  {
    // invalid
  }
  break;

  case 0x036e:
  {
    // invalid
  }
  break;

  case 0x036f:
  {
    // invalid
  }
  break;

  case 0x0370:
  {
    // invalid
  }
  break;

  case 0x0371:
  {
    // invalid
  }
  break;

  case 0x0372:
  {
    // invalid
  }
  break;

  case 0x0373:
  {
    // invalid
  }
  break;

  case 0x0374:
  {
    // invalid
  }
  break;

  case 0x0375:
  {
    // invalid
  }
  break;

  case 0x0376:
  {
    // invalid
  }
  break;

  case 0x0377:
  {
    // invalid
  }
  break;

  case 0x0378:
  {
    // invalid
  }
  break;

  case 0x0379:
  {
    // invalid
  }
  break;

  case 0x037a:
  {
    // invalid
  }
  break;

  case 0x037b:
  {
    // invalid
  }
  break;

  case 0x037c:
  {
    // invalid
  }
  break;

  case 0x037d:
  {
    // invalid
  }
  break;

  case 0x037e:
  {
    // invalid
  }
  break;

  case 0x037f:
  {
    // invalid
  }
  break;

  case 0x0380:
  {
    // invalid
  }
  break;

  case 0x0381:
  {
    // invalid
  }
  break;

  case 0x0382:
  {
    // invalid
  }
  break;

  case 0x0383:
  {
    // invalid
  }
  break;

  case 0x0384:
  {
    // invalid
  }
  break;

  case 0x0385:
  {
    // invalid
  }
  break;

  case 0x0386:
  {
    // invalid
  }
  break;

  case 0x0387:
  {
    // invalid
  }
  break;

  case 0x0388:
  {
    // invalid
  }
  break;

  case 0x0389:
  {
    // invalid
  }
  break;

  case 0x038a:
  {
    // invalid
  }
  break;

  case 0x038b:
  {
    // invalid
  }
  break;

  case 0x038c:
  {
    // invalid
  }
  break;

  case 0x038d:
  {
    // invalid
  }
  break;

  case 0x038e:
  {
    // invalid
  }
  break;

  case 0x038f:
  {
    // invalid
  }
  break;

  case 0x0390:
  {
    // invalid
  }
  break;

  case 0x0391:
  {
    // invalid
  }
  break;

  case 0x0392:
  {
    // invalid
  }
  break;

  case 0x0393:
  {
    // invalid
  }
  break;

  case 0x0394:
  {
    // invalid
  }
  break;

  case 0x0395:
  {
    // invalid
  }
  break;

  case 0x0396:
  {
    // invalid
  }
  break;

  case 0x0397:
  {
    // invalid
  }
  break;

  case 0x0398:
  {
    // invalid
  }
  break;

  case 0x0399:
  {
    // invalid
  }
  break;

  case 0x039a:
  {
    // invalid
  }
  break;

  case 0x039b:
  {
    // invalid
  }
  break;

  case 0x039c:
  {
    // invalid
  }
  break;

  case 0x039d:
  {
    // invalid
  }
  break;

  case 0x039e:
  {
    // invalid
  }
  break;

  case 0x039f:
  {
    // invalid
  }
  break;

  case 0x03a0:
  {
    // invalid
  }
  break;

  case 0x03a1:
  {
    // invalid
  }
  break;

  case 0x03a2:
  {
    // invalid
  }
  break;

  case 0x03a3:
  {
    // invalid
  }
  break;

  case 0x03a4:
  {
    // invalid
  }
  break;

  case 0x03a5:
  {
    // invalid
  }
  break;

  case 0x03a6:
  {
    // invalid
  }
  break;

  case 0x03a7:
  {
    // invalid
  }
  break;

  case 0x03a8:
  {
    // invalid
  }
  break;

  case 0x03a9:
  {
    // invalid
  }
  break;

  case 0x03aa:
  {
    // invalid
  }
  break;

  case 0x03ab:
  {
    // invalid
  }
  break;

  case 0x03ac:
  {
    // invalid
  }
  break;

  case 0x03ad:
  {
    // invalid
  }
  break;

  case 0x03ae:
  {
    // invalid
  }
  break;

  case 0x03af:
  {
    // invalid
  }
  break;

  case 0x03b0:
  {
    // invalid
  }
  break;

  case 0x03b1:
  {
    // invalid
  }
  break;

  case 0x03b2:
  {
    // invalid
  }
  break;

  case 0x03b3:
  {
    // invalid
  }
  break;

  case 0x03b4:
  {
    // invalid
  }
  break;

  case 0x03b5:
  {
    // invalid
  }
  break;

  case 0x03b6:
  {
    // invalid
  }
  break;

  case 0x03b7:
  {
    // invalid
  }
  break;

  case 0x03b8:
  {
    // invalid
  }
  break;

  case 0x03b9:
  {
    // invalid
  }
  break;

  case 0x03ba:
  {
    // invalid
  }
  break;

  case 0x03bb:
  {
    // invalid
  }
  break;

  case 0x03bc:
  {
    // invalid
  }
  break;

  case 0x03bd:
  {
    // invalid
  }
  break;

  case 0x03be:
  {
    // invalid
  }
  break;

  case 0x03bf:
  {
    // invalid
  }
  break;

  case 0x03c0:
  {
    // invalid
  }
  break;

  case 0x03c1:
  {
    // invalid
  }
  break;

  case 0x03c2:
  {
    // invalid
  }
  break;

  case 0x03c3:
  {
    // invalid
  }
  break;

  case 0x03c4:
  {
    // invalid
  }
  break;

  case 0x03c5:
  {
    // invalid
  }
  break;

  case 0x03c6:
  {
    // invalid
  }
  break;

  case 0x03c7:
  {
    // invalid
  }
  break;

  case 0x03c8:
  {
    // invalid
  }
  break;

  case 0x03c9:
  {
    // invalid
  }
  break;

  case 0x03ca:
  {
    // invalid
  }
  break;

  case 0x03cb:
  {
    // invalid
  }
  break;

  case 0x03cc:
  {
    // invalid
  }
  break;

  case 0x03cd:
  {
    // invalid
  }
  break;

  case 0x03ce:
  {
    // invalid
  }
  break;

  case 0x03cf:
  {
    // invalid
  }
  break;

  case 0x03d0:
  {
    // invalid
  }
  break;

  case 0x03d1:
  {
    // invalid
  }
  break;

  case 0x03d2:
  {
    // invalid
  }
  break;

  case 0x03d3:
  {
    // invalid
  }
  break;

  case 0x03d4:
  {
    // invalid
  }
  break;

  case 0x03d5:
  {
    // invalid
  }
  break;

  case 0x03d6:
  {
    // invalid
  }
  break;

  case 0x03d7:
  {
    // invalid
  }
  break;

  case 0x03d8:
  {
    // invalid
  }
  break;

  case 0x03d9:
  {
    // invalid
  }
  break;

  case 0x03da:
  {
    // invalid
  }
  break;

  case 0x03db:
  {
    // invalid
  }
  break;

  case 0x03dc:
  {
    // invalid
  }
  break;

  case 0x03dd:
  {
    // invalid
  }
  break;

  case 0x03de:
  {
    // invalid
  }
  break;

  case 0x03df:
  {
    // invalid
  }
  break;

  case 0x03e0:
  {
    // invalid
  }
  break;

  case 0x03e1:
  {
    // invalid
  }
  break;

  case 0x03e2:
  {
    // invalid
  }
  break;

  case 0x03e3:
  {
    // invalid
  }
  break;

  case 0x03e4:
  {
    // invalid
  }
  break;

  case 0x03e5:
  {
    // invalid
  }
  break;

  case 0x03e6:
  {
    // invalid
  }
  break;

  case 0x03e7:
  {
    // invalid
  }
  break;

  case 0x03e8:
  {
    // invalid
  }
  break;

  case 0x03e9:
  {
    // invalid
  }
  break;

  case 0x03ea:
  {
    // invalid
  }
  break;

  case 0x03eb:
  {
    // invalid
  }
  break;

  case 0x03ec:
  {
    // invalid
  }
  break;

  case 0x03ed:
  {
    // invalid
  }
  break;

  case 0x03ee:
  {
    // invalid
  }
  break;

  case 0x03ef:
  {
    // invalid
  }
  break;

  case 0x03f0:
  {
    // invalid
  }
  break;

  case 0x03f1:
  {
    // invalid
  }
  break;

  case 0x03f2:
  {
    // invalid
  }
  break;

  case 0x03f3:
  {
    // invalid
  }
  break;

  case 0x03f4:
  {
    // invalid
  }
  break;

  case 0x03f5:
  {
    // invalid
  }
  break;

  case 0x03f6:
  {
    // invalid
  }
  break;

  case 0x03f7:
  {
    // invalid
  }
  break;

  case 0x03f8:
  {
    // invalid
  }
  break;

  case 0x03f9:
  {
    // invalid
  }
  break;

  case 0x03fa:
  {
    // invalid
  }
  break;

  case 0x03fb:
  {
    // invalid
  }
  break;

  case 0x03fc:
  {
    // invalid
  }
  break;

  case 0x03fd:
  {
    // invalid
  }
  break;

  case 0x03fe:
  {
    // invalid
  }
  break;

  case 0x03ff:
  {
    // invalid
  }
  break;


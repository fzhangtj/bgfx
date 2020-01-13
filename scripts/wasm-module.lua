local codegen = require "codegen"
local idl = codegen.idl "bgfx.idl"
local fbsconfig = require "bindings-fbs-config"
local c_template = [[

#include<bgfx/c99/bgfx.h>
#include "m3_api_bgfx.h"

#include "m3_api_defs.h"
#include "m3_env.h"
#include "m3_exception.h"

static void* whandle;

$types

$funcs

static
M3Result SuppressLookupFailure(M3Result i_result)
{
    if (i_result == m3Err_functionLookupFailed)
        return m3Err_none;
    else
        return i_result;
}

M3Result  m3_LinkBGFX  (IM3Module module, void* handle)
{
    M3Result result = m3Err_none;
    whandle = handle;

$funcslink

_catch:
    return result;
}
]]

local function find_types(name)
    for _, typ in ipairs(idl.types) do
        if typ.cname == name then
            return typ
        end
    end
    return nil
end

local function hasPrefix(str, prefix)
	return prefix == "" or str:sub(1, #prefix) == prefix
end

local function hasSuffix(str, suffix)
	return suffix == "" or str:sub(-#suffix) == suffix
end

local function convert_type_0(arg)

	if hasPrefix(arg.ctype, "uint64_t") then
		return arg.ctype:gsub("uint64_t", "ulong")
	elseif hasPrefix(arg.ctype, "int64_t") then
		return arg.ctype:gsub("int64_t", "long")
	elseif hasPrefix(arg.ctype, "uint32_t") then
		return arg.ctype:gsub("uint32_t", "uint")
	elseif hasPrefix(arg.ctype, "int32_t") then
		return arg.ctype:gsub("int32_t", "int")
	elseif hasPrefix(arg.ctype, "uint16_t") then
		return arg.ctype:gsub("uint16_t", "ushort")
	elseif hasPrefix(arg.ctype, "bgfx_view_id_t") then
		return arg.ctype:gsub("bgfx_view_id_t", "ushort")
	elseif hasPrefix(arg.ctype, "uint8_t") then
		return arg.ctype:gsub("uint8_t", "byte")
	elseif hasPrefix(arg.ctype, "uintptr_t") then
		return arg.ctype:gsub("uintptr_t", "UIntPtr")
	elseif arg.ctype == "bgfx_caps_gpu_t" then
	    return arg.ctype:gsub("bgfx_caps_gpu_t", "uint")
	elseif arg.ctype == "const char*" then
		return "[MarshalAs(UnmanagedType.LPStr)] string"
	elseif hasPrefix(arg.ctype, "char") then
		return arg.ctype:gsub("char", "byte")
	elseif hasSuffix(arg.fulltype, "Handle") then
		return arg.fulltype
	elseif arg.ctype == "..." then
		return "[MarshalAs(UnmanagedType.LPStr)] string args"
	elseif arg.ctype == "va_list"
		or arg.fulltype == "bx::AllocatorI*"
		or arg.fulltype == "CallbackI*"
		or arg.fulltype == "ReleaseFn" then
		return "IntPtr"
	elseif arg.fulltype == "const ViewId*" then
		return "ushort*"
	end

	return arg.fulltype
end

-- bgfx_release_fn_t
local function convert_wasm_type(arg)
    local typ = find_types(arg.ctype)
    local ctype = arg.ctype:gsub("const", "")
    ctype = ctype:gsub(" ", "")
    if hasSuffix(ctype, "*") then
        return "ptr32"
    elseif ctype == "uint16_t" or ctype == "int16_t" or ctype == "bool" or ctype == "int8_t"
        or ctype == "uint8_t" or ctype == "uint32_t" then
        return "int32_t"
    elseif ctype == "uint64_t" then
        return "int64_t"
    elseif ctype == "bgfx_release_fn_t" then
        return "int32_t"    
    elseif hasSuffix(ctype, "_handle_t") then
        return "ptr32"
    elseif typ ~= nil and (typ.bits ~= nil or hasSuffix(typ.name, "::Enum")) then
        return "int32_t"
    elseif arg.ctype == "..." then
        return "ptr32"
    elseif hasPrefix(arg.ctype, "bgfx_view_id_t") then
        return "int32_t"   
    elseif hasPrefix(arg.ctype, "uintptr_t") then
		return "ptr32"   
    elseif arg.ctype == "va_list"
        or arg.fulltype == "bx::AllocatorI*"
        or arg.fulltype == "CallbackI*"
        or arg.fulltype == "ReleaseFn" then
        return "ptr32"    
    else
        return ctype
    end
end

local function wasm_type_symbol(typename)
    if typename == "int32_t" then
        return "i"
    elseif typename == "int64_t" then
        return "I"
    elseif typename == "float" then
        return "f"
    elseif typename == "double" then
        return "F"
    elseif typename == "ptr32" then
        return "*" 
    else
        assert(false, "unknow typename " .. typename)
    end     
end

local function convert_type(arg)
	local ctype = convert_type_0(arg)
	ctype = ctype:gsub("::Enum", "")
	ctype = ctype:gsub("const ", "")
	ctype = ctype:gsub(" &", "*")
	ctype = ctype:gsub("&", "*")
	return ctype
end

local function convert_struct_type(arg)
	local ctype = convert_type(arg)
	if hasPrefix(arg.ctype, "bool") then
		ctype = ctype:gsub("bool", "byte")
	end
	return ctype
end

local function convert_ret_type(arg)
	local ctype = convert_type(arg)
	if hasPrefix(ctype, "[MarshalAs(UnmanagedType.LPStr)]") then
		return "IntPtr"
	end
	return ctype
end

local converter = {}
local yield = coroutine.yield
local indent = ""

local gen = {}

function gen.gen_c()
	local r = c_template:gsub("$(%l+)", function(what)
        local tmp = {}
        local section
        if what == 'funcslink' then
            section = "funcs"
        else 
            section = what
        end
		for _, object in ipairs(idl[section]) do
			local co = coroutine.create(converter[what])
			local any
			while true do
				local ok, v = coroutine.resume(co, object)
				assert(ok, debug.traceback(co, v))
				if not v then
					break
				end
				table.insert(tmp, v)
				any = true
			end
			if any and tmp[#tmp] ~= "" then
				table.insert(tmp, "")
			end
		end
		return table.concat(tmp, "\n\t")
	end)
	return r
end

local combined = { "State", "Stencil", "Buffer", "Texture", "Sampler", "Reset" }

for _, v in ipairs(combined) do
	combined[v] = {}
end

local lastCombinedFlag

local function FlagBlock(typ)
	local format = "0x%08x"
	local enumType = " : uint"
	if typ.bits == 64 then
		format = "0x%016x"
		enumType = " : ulong"
	elseif typ.bits == 16 then
		format = "0x%04x"
		enumType = " : ushort"
	end

	yield("[Flags]")
	yield("public enum " .. typ.name .. "Flags" .. enumType)
	yield("{")

	for idx, flag in ipairs(typ.flag) do

		if flag.comment ~= nil then
			if idx ~= 1 then
				yield("")
			end

			yield("\t/// <summary>")
			for _, comment in ipairs(flag.comment) do
				yield("\t/// " .. comment)
			end
			yield("\t/// </summary>")
		end

		local flagName = flag.name:gsub("_", "")
		yield("\t"
			.. flagName
			.. string.rep(" ", 22 - #(flagName))
			.. " = "
			.. string.format(flag.format or format, flag.value)
			.. ","
			)
	end

	if typ.shift then
		yield("\t"
			.. "Shift"
			.. string.rep(" ", 22 - #("Shift"))
			.. " = "
			.. flag.shift
			)
	end

	-- generate Mask
	if typ.mask then
		yield("\t"
			.. "Mask"
			.. string.rep(" ", 22 - #("Mask"))
			.. " = "
			.. string.format(format, flag.mask)
			)
	end

	yield("}")
end

local function lastCombinedFlagBlock()
	if lastCombinedFlag then
		local typ = combined[lastCombinedFlag]
		if typ then
			FlagBlock(combined[lastCombinedFlag])
			yield("")
		end
		lastCombinedFlag = nil
	end
end

local enum = {}

local function convert_array(member)
	if string.find(member.array, "::") then
		return string.format("[%d]", enum[member.array])
	else
		return member.array
	end
end

local function convert_struct_member(member)
	if member.array then
		return "fixed " .. convert_struct_type(member) .. " " .. member.name .. convert_array(member)
	else
		return convert_struct_type(member) .. " " .. member.name
	end
end

local namespace = ""
local function need_convert(typ)
    if typ.handle then
        return false
    end
    if not typ.struct then
        return false
    end
    for _, member in ipairs(typ.struct) do
        if hasSuffix(member.ctype, "*") then
            return true
        end
        local memtyp = find_types(member.ctype)
        if memtyp ~= nil then
            local member_need = need_convert(memtyp)
            if member_need then
                return true
            end
        end

    end

    return false
end

local function convertedname(name)
    return "wa_" .. name
end

function converter.types(typ)
   
    if not need_convert(typ) then
        return
    else    
        print("type need to convert " .. typ.cname)
    end
   
    if true then
        return 
    end
    assert(typ.struct ~= nil)

	local skip = false

    local sname = typ.cname:gsub("_t", "_s")
    yield("typedef struct " .. convertedname(sname))
	-- if typ.namespace ~= nil then
	-- 		if namespace ~= typ.namespace then
	-- 			yield("public unsafe struct " .. typ.namespace)
	-- 			yield("{")
	-- 			namespace = typ.namespace
	-- 			indent = "\t"
	-- 		end
	-- 	elseif namespace ~= "" then
	-- 		indent = ""
	-- 		namespace = ""
	-- 		skip = true
	-- 	end

	-- 	if not skip then
	-- 		yield(indent .. "public unsafe struct " .. typ.name)
	-- 		yield(indent .. "{")
	-- 	end

    -- for _, member in ipairs(typ.struct) do
    --     local memtype = find_types(member.ctype)
    --     if hasSuffix(member.ctype, "*") then 
    --         yield("\tptr32 " .. member.name .. ";")
    --     elseif memtype ~= nil and need_convert(memtype) then
    --         yield(convertedname(member.ctype) .. " " .. member.name .. ";")
    --     else
    --         yield(member.ctype .. " " .. member.name .. ";")
    --     end
	-- end

    -- yield("} wa_" .. typ.cname)
    
    -- yield("\n\nstatic void wa_to_" .. typ.cname .. "(" .. typ.cname .. "* dest, " 
    --     .. convertedname(typ.name) .. "* src) {")
    --     for _, member in ipairs(typ.struct) do
    --         local memtype = find_types(member.ctype)
    --         if hasSuffix(member.ctype, "*") then 
    --             yield("\tptr_32 " .. member.name .. ";")
    --         elseif memtype ~= nil and need_convert(memtype) then
    --             yield(convertedname(member.ctype) .. " " .. member.name .. ";")
    --         else
    --             yield(member.ctype .. " " .. member.name .. ";")
    --         end
    --         if member->sc
    --         yield("dest->" .. member.name .. " = " .. "src->")
    --     end
    -- yield("}")

end

function converter.funcs(func)

	if func.cpponly or fbsconfig.funcs_gen[func.cname]==nil then
		return
	end

    yield("m3ApiRawFunction(m3_bgfx_" .. func.cname .. ")")
    yield("{")


    local wasmtype = convert_wasm_type(func.ret)
    if func.ret.ctype ~= 'void' then
        yield("m3ApiReturnType(" .. wasmtype .. ")")
    end

    local invoke_args = {}
    if func.this ~= nil then
        yield("\tm3ApiGetArgMem   (" .. func.this_type.ctype .. "       , _bgfx_this)")
		table.insert(invoke_args, "_bgfx_this")
    end
    

    for _, arg in ipairs(func.args) do
        print(arg.ctype)
        -- if arg.ctype == "int32_t" or arg.ctype == "bool" 
        if hasSuffix(arg.ctype, "bgfx_init_t *") or hasSuffix(arg.ctype, "bgfx_init_t*")  then
			yield("\tm3BgfxApiGetInitArg(_bgfx_" .. arg.name .. ")")
		elseif hasSuffix(arg.ctype, "platform_data_t *") or hasSuffix(arg.ctype, "platform_data_t*")  then
            yield("\tm3BgfxApiGetPlatformDataArg(_bgfx_" .. arg.name .. ")")	
        elseif hasSuffix(arg.ctype, "*") then
            yield("\tm3ApiGetArgMem   (" .. arg.ctype .. "       , _bgfx_" .. arg.name .. ")")
        else
            yield("\tm3ApiGetArg   (" .. arg.ctype .. "       , _bgfx_" .. arg.name .. ")")
            
        end
        table.insert(invoke_args, "_bgfx_" .. arg.name)
	    -- table.insert(args, convert_type(arg) .. " " .. arg.name)
	end
    
    local invoke = "bgfx_" .. func.cname .. "(" .. table.concat(invoke_args, ", ") .. ");"
    if func.ret.ctype == 'void' then
        yield(invoke)
    else
        yield(func.ret.ctype .. " ret = " .. invoke)
        local ret = "ret"
        if (hasSuffix(func.ret.ctype, "_handle_t")) then
           ret = "bgfx_handle_to_idx(" .. ret .. ")"
        -- elseif hasSuffix(func.ret.ctype, "*") then
        -- ret = "bgfx_handle_to_idx(" .. ret .. ")"
        end
        yield("m3ApiReturn(" .. ret .. ")")
    end
    --yield("ret " .. func.ret.ctype)
    yield ("return m3Err_none;")
    yield("}")
end

function converter.funcslink(func)
    if func.cpponly or fbsconfig.funcs_gen[func.cname]==nil then
		return
	end


    local args = {}
    local ret = "v"

    if func.ret.ctype ~= 'void' then
        ret = wasm_type_symbol(convert_wasm_type(func.ret))
    end

    if func.this ~= nil then
        table.insert(args, wasm_type_symbol(convert_wasm_type(func.this_type)))
    end
    
    for _, arg in ipairs(func.args) do
        table.insert(args, wasm_type_symbol(convert_wasm_type(arg)))
    end
    
    local signature = ret .. "(" .. table.concat(args, "") .. ")"
    local exportname = "bgfx_" .. func.cname
    local funcname = "m3_bgfx_" .. func.cname
    yield("   (SuppressLookupFailure (m3_LinkRawFunction (module, \"env\", \"" .. exportname .. "\", \"" .. 
        signature .. "\", &" .. funcname .. ")));")
end
-- printtable("idl types", idl.types)
-- printtable("idl funcs", idl.funcs)

function gen.write(codes, outputfile)
	local out = assert(io.open(outputfile, "wb"))
	out:write(codes)
	out:close()
	print("Generating: " .. outputfile)
end

if (...) == nil then
	-- run `lua bindings-cs.lua` in command line
	print(gen.gen())
end

return gen

local codegen = require "codegen"
local fbsconfig = require "bindings-fbs-config"
local idl = codegen.idl "bgfx.idl"

local fbs_schema_template = [[
// AUTO GENERATED! DO NOT EDIT!
namespace Bgfx.Bridge;

$types

enum CommandType: ushort {
    invalid = 0,
    $cmdtypes
}

$argdefs

union CommandArgs { 
    $cmdargs
}

table Command {
    id: int;
    method: CommandType = invalid;
    args: CommandArgs;
}

table CommandBatch {
    commands:[Command]; 
}

root_type CommandBatch;
root_type Command;

]]

local cs_template = [[
	$funcs
]]

local cpp_caller_template = [[
$funcs
]]

local cpp_native_template = [[
#include<bgfx/c99/bgfx.h>
#include "bgfx_custom.h"
#include "bgfx_generated.h"
$funcs

static void __initCmdHandlers() {
    for (int i = 0;i < 0xffff;++i) {
        command_handlers[i] = &mock;
	}

	$handlers
}

]]


local function underscorecase_to_camelcase(name)
	local tmp = {}
	for v in name:gmatch "[^_]+" do
		tmp[#tmp+1] = v:sub(1,1):upper() .. v:sub(2)
	end
	return table.concat(tmp)
end


local function hasPrefix(str, prefix)
	return prefix == "" or str:sub(1, #prefix) == prefix
end

local function hasSuffix(str, suffix)
	return suffix == "" or str:sub(-#suffix) == suffix
end


local function fbs_type_name_from_name(namespace, name) 
    if namespace ~= nil then 
        return namespace .. '__' .. name
    else
        return name
    end
end 

local function has_value(tab, val)
    for index, value in ipairs(tab) do
        if value == val then
            return true
        end
    end

    return false
end

local function convert_type_0(arg)

	if arg.ctype == "const char*" then
		return "string"
	end

    if string.find(arg.ctype, "*", 1) ~= nil then
        return  'ulong'
    end

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
		return arg.ctype:gsub("uintptr_t", "uint")
	elseif arg.ctype == "bgfx_caps_gpu_t" then
	    return arg.ctype:gsub("bgfx_caps_gpu_t", "uint")
	elseif arg.ctype == "const char*" then
		return "string"
	elseif hasPrefix(arg.ctype, "char") then
		return arg.ctype:gsub("char", "byte")
	elseif hasSuffix(arg.fulltype, "Handle") then
		return 'ushort'
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

local scalar_types = {"long", "ulong", "double", "int", "uint", "float", "short", "ushort", "byte", "ubyte", "bool"}
for _,value in ipairs(scalar_types) 
do
    scalar_types[value] = true
end



local fbs_struct_types = {}



local function convert_type(arg)
    local ctype = convert_type_0(arg)   
	ctype = ctype:gsub("::Enum", "")
	ctype = ctype:gsub("const ", "")
	ctype = ctype:gsub(" &", "*")
	ctype = ctype:gsub("&", "*")
	return ctype
end

local function fbs_type_name(typ)
	local name = typ.typename or typ.name
    return fbs_type_name_from_name(typ.namespace, name)
end 

local type_defs = {}
for _, typ in ipairs(idl.types) do
	print("types " .. fbs_type_name(typ))
    type_defs[fbs_type_name(typ)] = typ
end   
    --type_defs[fbs_type_name(typ)] = typ


local function convert_struct_type(arg)
    -- print('full type ' .. arg.fulltype)
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


--- bgfx_set_view_transform(bgfx_view_id_t _id, const void* _view, const void* _proj)
local function command_name_for_func(func)
    return func.cname 
end    

-- function command_args_for_func(func)
-- end

-- function command_args_def_for_func(func)
-- end
function gen.genclient() 
    local r = cpp_caller_template:gsub("$(%l+)", function(what)
        local tmp = {}
        
        for _, object in ipairs(idl[what]) do
            local co = coroutine.create(converter.client)
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

function gen.gennative() 
    local r = cpp_native_template:gsub("$(%l+)", function(what)
        local tmp = {}
        
		for _, object in ipairs(idl.funcs) do
			--print("--8---".. "native_" .. what)
            local co = coroutine.create(converter["native_" .. what])
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
-- 
function gen.gencs()
	local r = cs_template:gsub("$(%l+)", function(what)
		local tmp = {}
		for _, object in ipairs(idl[what]) do
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

function gen.gen()

	local r = fbs_schema_template:gsub("$(%l+)", function(what)
        local tmp = {}
        print("xxxreplace  " .. what)
        if what == 'cmdtypes' then
            for _, func in ipairs(idl["funcs"]) do
                if not func.cpponly and fbsconfig.funcs_gen[func.cname]~=nil then
                    table.insert(tmp, command_name_for_func(func) .. ',')
                end    
            end
        elseif what == "argdefs" then
            for _, object in ipairs(idl['funcs']) do
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
        elseif what == "cmdargs"   then 
            for _, object in ipairs(idl['funcs']) do
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
        else
            for _, object in ipairs(idl[what]) do
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

	-- yield("[Flags]")
	yield("enum " .. typ.name .. "Flags" .. enumType)
	yield("{")

    local function sortfunc(a, b)
        return string.format("0x%016x", a.value) < string.format("0x%016x", b.value)
    end

    local flag = typ.flag;
    table.sort(flag, sortfunc)
--    added = {}
    -- print('typ.flag ' .. ' ' .. _type)
    local generated = {}
	for idx, flag in ipairs(flag) do

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
        if not generated[flag.value] then
            generated[flag.value] = true
            print(flagName .. ' ' .. flag.value)
            yield("\t"
			.. flagName
			.. string.rep(" ", 22 - #(flagName))
			.. " = "
			.. string.format(flag.format or format, flag.value)
			.. ","
			)
        end
		
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
	-- if typ.mask then
	-- 	yield("\t"
	-- 		.. "Mask"
	-- 		.. string.rep(" ", 22 - #("Mask"))
	-- 		.. " = "
	-- 		.. string.format(format, flag.mask)
	-- 		)
	-- end

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

local function convert_struct_member(namespace, member)
    local name = convert_struct_type(member)
    local name_with_namespace = fbs_type_name_from_name(namespace, convert_struct_type(member))
    if type_defs[name_with_namespace] ~= nil then
        name  = name_with_namespace
    end
	if member.array then
		return member.name .. ":" .. "[" .. name .. "]"
    else
		return  member.name .. ":" .. name
	end
end

local function get_member_type(namespace, member)
    local name = fbs_type_name_from_name(namespace, convert_struct_type(member)) 
    if type_defs[name] ~= nil then
        return type_defs[name]
    end
    return type_defs[convert_struct_type(member)]
end    

local function is_fbs_struct(fsbtype_name)
    local typ = type_defs[fsbtype_name]
    assert(typ ~= nil, "type not found " .. fsbtype_name)
    assert(typ.struct ~= nil, "type not struct " .. fsbtype_name)
    print("begin is_fbs_struct " .. fsbtype_name)
    if fbs_struct_types[fsbtype_name] ~= nil then
        return fbs_struct_types[fsbtype_name]
    end
    
    local valid = true
    for _, member in ipairs(typ.struct) do
        --print('***' .. convert_struct_type(member))
        local member_type = get_member_type(fsbtype_name, member)
        -- if member_type~=nil then
        --     print('mem type' .. member_type.name .. '  ' .. member_type.struct)
        -- end
		print(member.name .. ' *' .. convert_struct_type(member) .. '*')
		if member_type ~= nil then -- and (hasSuffix(member_type.name, "::Enum")  or member_type.bits ~= nil) then
			print('xxxxxx ' .. member_type.name)
		end	
		if member.array then
            print('array')
            valid = false
            break
        elseif member_type ~= nil and member_type.struct ~= nil  then
			
			-- get_member_type(namespace, member)
			local mem_fbs_typname = fbs_type_name(member_type)
			print('22222 ' .. mem_fbs_typname)
            if not is_fbs_struct(mem_fbs_typname) then
                print('struc')
                valid = false
                break
            end
        elseif member_type == nil and scalar_types[convert_struct_type(member)] == nil then  
            print('3333')
            print("not scalar " .. convert_struct_type(member) .. " " .. member.name)
            valid = false
            break
        end
    end

    print("end is_fbs_struct " .. fsbtype_name .. " ")
    fbs_struct_types[fsbtype_name] = valid
    return fbs_struct_types[fsbtype_name]

end

local namespace = ""

 

function converter.types(typ)
	if typ.handle then
		lastCombinedFlagBlock()
        yield("")
--		yield("struct " .. typ.name .. "{ idx:ushort; }")
	elseif hasSuffix(typ.name, "::Enum") then
		-- lastCombinedFlagBlock()

		yield("enum " .. typ.typename .. ":byte")
		yield("{")
		for idx, enum in ipairs(typ.enum) do

			if enum.comment ~= nil then
				if idx ~= 1 then
					yield("")
				end

				yield("\t/// <summary>")
				for _, comment in ipairs(enum.comment) do
					yield("\t/// " .. comment)
				end
				yield("\t/// </summary>")
			end

			yield("\t" .. enum.name .. ",")
		end
		yield("");
		yield("\tCount")
		yield("}")

		enum["[" .. typ.typename .. "::Count]"] = #typ.enum

	elseif typ.bits ~= nil then
		local prefix, name = typ.name:match "(%u%l+)(.*)"
		if prefix ~= lastCombinedFlag then
			lastCombinedFlagBlock()
			lastCombinedFlag = prefix
		end
		local combinedFlag = combined[prefix]
		if combinedFlag then
			combinedFlag.bits = typ.bits
			combinedFlag.name = prefix
			local flags = combinedFlag.flag or {}
			combinedFlag.flag = flags
			local lookup = combinedFlag.lookup or {}
			combinedFlag.lookup = lookup
			for _, flag in ipairs(typ.flag) do
				local flagName = name .. flag.name:gsub("_", "")
				local value = flag.value
				if value == nil then
					-- It's a combined flag
					value = 0
					for _, v in ipairs(flag) do
						value = value | assert(lookup[name .. v], v .. " is not defined for " .. flagName)
					end
				end
				lookup[flagName] = value
				table.insert(flags, {
					name = flagName,
					value = value,
					comment = flag.comment,
				})
			end

			if typ.shift then
				table.insert(flags, {
					name = name .. "Shift",
					value = typ.shift,
					format = "%d",
					comment = typ.comment,
				})
			end

			if typ.mask then
				-- generate Mask
				table.insert(flags, {
					name = name .. "Mask",
					value = typ.mask,
					comment = typ.comment,
				})
				lookup[name .. "Mask"] = typ.mask
			end
		else
			FlagBlock(typ)
		end
	elseif typ.struct ~= nil then

		local skip = false

        local fbstyp = 'table'
        if is_fbs_struct(fbs_type_name(typ)) then
            fbstyp = 'struct'
        end
        local fbs_name = fbs_type_name(typ)
		
        yield(fbstyp .. " " .. fbs_name .. " {")

        local has_members = false
        for _, member in ipairs(typ.struct) do
            has_members = true
			yield(
				"\t" .. convert_struct_member(typ.name, member) .. ";"
                )
        end
        
        if not has_members then
            yield(
				"\t" .. "dummy:byte;"
                )
        end

		yield("}")
	end
end

local function get_arg_members(func)
	local members = {}
	local config = fbsconfig.func_gen_config[func.cname] or {}
	if config.genobj then
		-- genexpression
		local member = {fulltype="ulong", name="_ret_obj", ctype=func.ret.ctype}
		if config.genexpression then
			member['genexpression'] = config.genexpression
		end
		table.insert(members, member)
	elseif func.this ~= nil then
        table.insert(members, {fulltype="ulong", name="_this", ctype=func.this})
        -- args[1] = func.this_type.type .. "* _this"
	end

	if hasSuffix(func.ret.fulltype, "Handle") then
		table.insert(members, {fulltype='ushort', name="_ret_handle", ctype=func.ret.ctype})
	end
	
-- vectors = {_attachment={length="_num", elem_size="sizeof(bgfx_attachment_t)"}},
-- 
	local skipParams = {}
	local vectors = config.vectors or {}	
	for _, vector in pairs(vectors) do 
		--print("vector.length" .. vector.length)
		if type(vector.length) == "string" then
			skipParams[vector.length] = true
		end
	end
	for _, arg in ipairs(func.args) do
		local member = nil
		if vectors[arg.name] ~= nil then
			--print("vector...." ..vectors[arg.name].length)
			member = {fulltype='[byte]', vector=true, name=arg.name, ctype=arg.ctype, 
			elem_size=vectors[arg.name].elem_size, length=vectors[arg.name].length}
		elseif skipParams[arg.name] == nil then
			member = {fulltype=convert_type(arg), name= arg.name, ctype=arg.ctype};
		end

		if member ~= nil then
			table.insert(members, member)
		end

		
	end
	
	local createobj = config.args_create_obj or {}
	for _, member in ipairs(members) do
		if has_value(createobj, member.name) then
			member["createobj"] = true
		end
	end
    return members
end

function converter.client(func)
	if func.cpponly or fbsconfig.funcs_gen[func.cname] == nil then
		return
    end

	-- if func.codetemp ~= nil then
	-- 	print("hastemp")
	-- end
	yield("DOTS_EXPORT(" .. func.ret.ctype .. ") bgfx_bridge_" .. func.cname .. "(" .. func.codetemp.CARGS .. ") {")
	local members = get_arg_members(func);

	for _, member in ipairs(members) do
		if member.vector then
			yield("auto _vector_" .. member.name .. "=" .. "builder.CreateVector<int8_t>(" .. "(int8_t*)" .. member.name .. ", (" 
				.. member.length .. ")*(" ..  member.elem_size .. "));")
		end
	end

	local args = {}
	
	local ret_handle = nil
	local ret_obj = nil
	for _, member in ipairs(members) do
		local expression = ""
		if (member.name == '_this') then
			expression = "(uint64_t)_this"
		elseif (member.name == '_ret_handle') then
			expression = "_ret_handle"
			ret_handle = member
		elseif (member.name == '_ret_obj') then
			expression = "(uint64_t)_ret_obj"
			ret_obj = member
		elseif member.ctype	~= nil and hasSuffix(member.ctype, "handle_t") then
			expression = member.name .. ".idx"
		elseif member.vector then 
			expression = "_vector_" .. member.name
		elseif member.fulltype == "string" then
			expression = "builder.CreateString(" .. member.name .. ")"
		elseif hasSuffix(member.ctype, "*") then
			expression =  "(uint64_t)" .. member.name
		else
			local arg_type = member.fulltype
			local cast = false
			if scalar_types[arg_type] == nil then
				arg_type = "Bgfx::Bridge::" .. arg_type
				cast = true
			end
			expression = member.name
			if cast then
				expression = "(" .. arg_type ..")".. expression
			end
		end

		args[#args+1] = expression
	end

	if ret_handle then
		yield("auto _ret_handle = createHandle((uint16_t)"..ret_handle.ctype.."_Type);")
	end
	if ret_obj then
		if ret_obj.genexpression then
			yield("auto _ret_obj = " .. ret_obj.genexpression .. ";")
		else
			yield("auto _ret_obj = (" ..func.ret.ctype.. ") new int;")
		end
	end

	local config = fbsconfig.func_gen_config[func.cname] or {}
	local isstruct = (config.vectors == nil)
	local argtypename =  underscorecase_to_camelcase(func.cname) .. "Args"
	local fullname = "Bgfx::Bridge::" .. argtypename

	for _, member in ipairs(members) do
		if member.fulltype == "string" then
			isstruct = false
			break
		end
	end

	if isstruct then
		yield("\tauto args = builder.CreateStruct<" .. fullname .. ">(" .. fullname .. "(" .. table.concat(args, ", ") .. "));")
	else
		yield("\tauto args = Bgfx::Bridge::Create" .. argtypename .. "(builder," .. table.concat(args, ", ") .. ");")
	end
	--yield("\tauto args = builder.CreateStruct<" .. fullname .. ">(" .. fullname .. "(" .. table.concat(args, ", ") .. "));")
	yield("\tenqueueRequest(")
	yield("\tBgfx::Bridge::CommandType::CommandType_" .. command_name_for_func(func).. ",") 
	yield("\tBgfx::Bridge::CommandArgs_" .. argtypename .. ",")
	yield("\targs.Union()")
	yield("\t);")
	if ret_handle then
		yield("return " .. ret_handle.ctype .. "{_ret_handle};");
	elseif ret_obj then
		yield("return _ret_obj;")
	elseif func.ret.ctype == 'uint32_t' then
		yield("return 0;")
	elseif func.ret.ctype ~= 'void' then
		yield("return _this;")
	end

	yield("}")
	
end   

function converter.funcs(func)
    
	if func.cpponly or fbsconfig.funcs_gen[func.cname] == nil then
		return
	end

	yield("[DllImport(DllName, EntryPoint=\"bgfx_bridge_" .. func.cname .. "\", CallingConvention = CallingConvention.Cdecl)]")

	if func.ret.cpptype == "bool" then
		yield("[return: MarshalAs(UnmanagedType.I1)]")
	end

	local args = {}
	if func.this ~= nil then
		args[1] = func.this_type.type .. "* _this"
	end
	for _, arg in ipairs(func.args) do
		table.insert(args, convert_type(arg) .. " " .. arg.name)
	end
	yield("public static extern unsafe " .. convert_ret_type(func.ret) .. " " .. func.cname
		.. "(" .. table.concat(args, ", ") .. ");")
end

function pointertype_to_type(type) 
	type = type:gsub("*", "")
	type = type:gsub(" ", "")
	type = type:gsub("const", "")
	return type
end

function converter.native_funcs(func)
	if func.cpponly or fbsconfig.funcs_gen[func.cname] == nil then
		return
	end
	
	local argtypename =  underscorecase_to_camelcase(func.cname) .. "Args"
	yield("static void handle_" .. func.cname .. "(const Bgfx::Bridge::Command* cmd) {")
	local argtypename =  underscorecase_to_camelcase(func.cname) .. "Args"
	yield("auto args = cmd->args_as_" .. argtypename .. "();")
	--auto args = cmd->args_as_VertexLayoutBeginArgs();
-- createobj

	local members = get_arg_members(func);
	for _, member in ipairs(members) do
		members[member.name] = member
	end

	for _, member in ipairs(members) do
		local expression = nil
		if member.createobj then
			local t = pointertype_to_type(member.ctype)
			yield("auto _created_" .. member.name .. " = create_object<" .. t .. ">(args->" .. member.name .. "());")
		end
	end

	local args = {}
	if func.this ~= nil then
		local _this = ""
		local member = members["_this"]
		if (member.createobj) then
			_this = "_created_" .. member.name 
		else
			_this = "get_object<" .. pointertype_to_type(member.ctype) .. ">(args->_this())"
		end

		yield("auto _this=" .. _this .. ";")
		table.insert(args, "_this")
		-- table.insert(args, _this)
	end

	for _, arg in ipairs(func.args) do
		local expression = nil
		local member = members[arg.name]
		if hasSuffix(arg.ctype, "handle_t") then
			expression = arg.ctype .. "{get_handle_idx(args->" .. arg.name .. "()," .. arg.ctype .. "_Type)}"
		elseif member ~= nil and member.fulltype == 'string' then
			expression = "args->".. arg.name .. "()->c_str()"
		elseif hasSuffix(arg.ctype, "*") then
			local member = members[arg.name]
			if member.vector then
				expression = "(" .. arg.ctype .. ")(args->" .. arg.name .. "()->Data())"
			else
				expression = "get_object<" .. pointertype_to_type(arg.ctype) .. ">(args->" .. arg.name .. "())"
			end
		elseif members[arg.name] == nil then
			for _, member in ipairs(members) do
				if member.vector and member.length == arg.name then
					expression = "(args->".. member.name .. "()->size() / (" .. member.elem_size .. "))"
				end
			end
		else
			expression = "(" .. arg.ctype .. ")" .. "args->" .. arg.name .. "()"
		end
		yield("auto " .. arg.name .. "=" .. expression .. ";")
		table.insert(args, arg.name)
		--table.insert(args, expression)
	end


	-- for _, member in ipairs(members) do
	-- 	if member.vector then
	-- 		yield("auto _vector_" .. member.name .. "=" .. "builder.CreateVector<int8_t>(" .. "(int8_t*)" .. member.name .. ", (" 
	-- 			.. member.length .. ")*(" ..  member.elem_size .. "));")
	-- 	end
	-- end
	
	local ret_handle = nil
	local _ret_obj = nil
	for _, member in ipairs(members) do
		if (member.name == '_ret_handle') then
			ret_handle = member
		end
		if (member.name == '_ret_obj') then
			_ret_obj = member
		end
	end

	local nativename = "bgfx_" .. func.cname;
	if fbsconfig.func_gen_config[func.cname] ~= nil and fbsconfig.func_gen_config[func.cname].native_method ~= nil then
		nativename = fbsconfig.func_gen_config[func.cname].native_method
	end
--put_handle_idx(uint16_t client_idx, uint16_t idx, uint16_t type) 
	if ret_handle ~= nil then
		yield("auto _ret_handle = " .. nativename .. "(" .. table.concat(args, ", ") .. ");")
		yield("put_handle_idx(args->" .. ret_handle.name .. '(), _ret_handle.idx, ' .. ret_handle.ctype .. '_Type);')
	elseif _ret_obj~= nil then
		yield("auto _ret_obj = " .. nativename .. "(" .. table.concat(args, ", ") .. ");")
		yield("put_object<" .. pointertype_to_type(_ret_obj.ctype) .. ">(args->" .. _ret_obj.name .. '(), _ret_obj);')
	else
		yield(nativename .. "(" .. table.concat(args, ", ") .. ");")
	end
	

	local argformat = {}

	if func.this ~= nil then
		table.insert(argformat, "_this=%p")
	end
	for _, arg in ipairs(func.args) do
		local fmt = nil
		local member = members[arg.name]
		if member ~= nil and member.fulltype == 'string' then
			fmt = arg.name .. "=%s"
		elseif hasSuffix(arg.ctype, "*") then
			fmt = arg.name .. "=%p"
		elseif arg.ctype == 'uint16_t' or  arg.ctype == 'uint32_t' then
			fmt = arg.name .. "=%u"
		elseif arg.ctype == 'int16_t' or  arg.ctype == 'int32_t' then
			fmt = arg.name .. "=%d"
		elseif arg.ctype == 'uint64_t' then
			fmt = arg.name .. "=%lu"
		elseif arg.ctype == 'int64_t' then
			fmt = arg.name .. "=%ld"	
		else
			fmt = arg.name .. "=%d"	
		end
		table.insert(argformat, fmt)
	end
	local format = table.concat(argformat, ", ")
	local argv = table.concat(args, ", ")
	if ret_handle ~= nil then
		format = format .. ", ret=%d"
		argv = argv .. ", _ret_handle.idx"
	elseif _ret_obj ~= nil then
		format = format .. ", ret=%p"
		argv = argv .. ", _ret_obj"
	end
	format = format
	
	--yield("NSLog(@\"BGFX_BRIDGE " .. func.cname .. "\");")
	-- yield("NSLog(@\"BGFX_BRIDGE " .. func.cname .. "  " .. format .. "\", " .. argv ..");")
	yield("}")

	

	-- for _, member in ipairs(members) do
	-- 	if member.vector then
	-- 		yield("auto _vector_" .. member.name .. "=" .. "builder.CreateVector<int8_t>(" .. "(int8_t*)" .. member.name .. ", (" 
	-- 			.. member.length .. ")*(" ..  member.elem_size .. "));")
	-- 	end
	-- end

	-- local args = {}
	
	-- local ret_handle = nil
	-- local ret_obj = nil
	-- for _, member in ipairs(members) do
	-- 	local expression = ""
	-- 	if (member.name == '_this') then
	-- 		expression = "(uint64_t)_this"
	-- 	elseif (member.name == '_ret_handle') then
	-- 		expression = "_ret_handle"
	-- 		ret_handle = member
	-- 	elseif (member.name == '_ret_obj') then
	-- 		expression = "(uint64_t)_ret_obj"
	-- 		ret_obj = member
	-- 	elseif member.ctype	~= nil and hasSuffix(member.ctype, "handle_t") then
	-- 		expression = member.name .. ".idx"
	-- 	elseif member.vector then 
	-- 		expression = "_vector_" .. member.name
	-- 	elseif hasSuffix(member.ctype, "*") then
	-- 		expression =  "(uint64_t)" .. member.name
	-- 	else
	-- 		local arg_type = member.fulltype
	-- 		local cast = false
	-- 		if scalar_types[arg_type] == nil then
	-- 			arg_type = "Bgfx::Bridge::" .. arg_type
	-- 			cast = true
	-- 		end
	-- 		expression = member.name
	-- 		if cast then
	-- 			expression = "(" .. arg_type ..")".. expression
	-- 		end
	-- 	end

	-- 	args[#args+1] = expression
	-- end

	-- if ret_handle then
	-- 	yield("auto _ret_handle = createHandle((uint16_t)"..ret_handle.ctype.."_Type);")
	-- end
	-- if ret_obj then
	-- 	if ret_obj.genexpression then
	-- 		yield("auto _ret_obj = " .. ret_obj.genexpression .. ";")
	-- 	else
	-- 		yield("auto _ret_obj = (" ..func.ret.ctype.. ") new int;")
	-- 	end
	-- end

	-- local config = fbsconfig.func_gen_config[func.cname] or {}
	-- local isstruct = (config.vectors == nil)
	-- local argtypename =  underscorecase_to_camelcase(func.cname) .. "Args"
	-- local fullname = "Bgfx::Bridge::" .. argtypename

	-- if isstruct then
	-- 	yield("\tauto args = builder.CreateStruct<" .. fullname .. ">(" .. fullname .. "(" .. table.concat(args, ", ") .. "));")
	-- else
	-- 	yield("\tauto args = Bgfx::Bridge::Create" .. argtypename .. "(builder," .. table.concat(args, ", ") .. ");")
	-- end
	-- --yield("\tauto args = builder.CreateStruct<" .. fullname .. ">(" .. fullname .. "(" .. table.concat(args, ", ") .. "));")
	-- yield("\tenqueueRequest(")
	-- yield("\tBgfx::Bridge::CommandType::CommandType_" .. command_name_for_func(func).. ",") 
	-- yield("\tBgfx::Bridge::CommandArgs_" .. argtypename .. ",")
	-- yield("\targs.Union()")
	-- yield("\t);")
	-- if ret_handle then
	-- 	yield("return " .. ret_handle.ctype .. "{_ret_handle};");
	-- elseif ret_obj then
	-- 	yield("return _ret_obj;")
	-- elseif func.ret.ctype == 'uint32_t' then
	-- 	yield("return 0;")
	-- elseif func.ret.ctype ~= 'void' then
	-- 	yield("return _this;")
	-- end

end

function converter.native_handlers(func)
	if func.cpponly or fbsconfig.funcs_gen[func.cname] == nil then
		return
    end
	local commandType = "Bgfx::Bridge::CommandType::CommandType_" .. command_name_for_func(func)
	yield("command_handlers[(ushort)" .. commandType .. "] = " .. "handle_" .. func.cname .. ";")
end


local function get_table_count(t)
    local count = 0
    for _, _ in pairs(t) do
        count = count + 1
    end
    return count
end

function converter.cmdargs(func)
    if func.cpponly or fbsconfig.funcs_gen[func.cname] == nil then
		return
    end

    local members = get_arg_members(func)
    if get_table_count(members) == 0 then
        return
    end
    yield(underscorecase_to_camelcase(func.cname) .. "Args,")
end

function converter.argdefs(func)
    if func.cpponly or fbsconfig.funcs_gen[func.cname] == nil then
		return
    end

    local members = get_arg_members(func)
    if get_table_count(members) == 0 then
        return
	end
	
	local fbstype = "struct"
	local config = fbsconfig.func_gen_config[func.cname] or {}
	if config.vectors then
		fbstype = "table"
	end
	for _, member in ipairs(members) do
		if member.fulltype == "string" then
			fbstype = "table"
		end
	end
    yield(fbstype .. " " .. underscorecase_to_camelcase(func.cname) .. "Args {")

    for _, member in ipairs(members) do
		yield("\t" .. member.name .. ":" .. member.fulltype .. ";")
    end

    yield("}")
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

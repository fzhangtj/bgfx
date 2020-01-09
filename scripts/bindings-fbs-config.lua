local config = {}

config.funcs_gen = {"copy", 
"set_debug",
"vertex_layout_begin",
"vertex_layout_add",
"vertex_layout_end",
"create_vertex_layout",
"create_shader",
"set_shader_name",
"create_program",
"create_uniform",
"create_texture_2d",
"create_index_buffer",
"create_vertex_buffer",
"encoder_begin",
"encoder_end",
"encoder_set_state",
"encoder_set_uniform",
"encoder_set_index_buffer",
"encoder_set_vertex_buffer",
"encoder_submit",
"frame",
"encoder_set_texture",
"set_state",
"set_transform",
"set_index_buffer",
"set_vertex_buffer",
"set_uniform",
"set_texture",
"submit",
"encoder_set_transform",
"set_view_transform",
"set_view_mode",
"set_view_rect",
"set_view_scissor",
"set_view_clear",
"set_view_frame_buffer",
"touch",
"create_frame_buffer_from_attachment",
"reset",
"dbg_text_clear",
"destroy_texture",
"request_screen_shot"
}

for _, v in ipairs(config.funcs_gen) do
	config.funcs_gen[v] = {}
end

config.func_gen_config = {
	encoder_begin = {genobj = true},
	copy = {
		genobj = true, 
		genexpression = "genMemory(_size)",
		vectors = {_data={length="_size", elem_size="1"}},
		-- args = {
		-- 	_data = {vector= {cname="_data", length="_size", elem_size="1"}}
		-- }
	},
	vertex_layout_begin = {
		args_create_obj = {"_this"}
	},
	-- uint8_t _num, const bgfx_attachment_t* _attachment
	create_frame_buffer_from_attachment = {
		vectors = {_attachment={length="_num", elem_size="sizeof(bgfx_attachment_t)"}},
		native_method = "__create_frame_buffer_from_attachment"
	},
	set_transform = {
		vectors = {_mtx={length="_num", elem_size="4*4*4"}},
	},
	encoder_set_transform = {
		vectors = {_mtx={length="_num", elem_size="4*4*4"}},
	},
	set_view_transform = {
		vectors = {_view={length=1, elem_size="4*4*4"}, _proj={length=1, elem_size="4*4*4"}},
	},
	set_uniform = {
		vectors = {_value={length="_num", elem_size="4*4*4"}},
	},
	encoder_set_uniform = {
		vectors = {_value={length="_num", elem_size="4*4*4"}},
	},
};



return config
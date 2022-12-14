REBOL [
    Title:   "Godot codecs and a scheme"
    type:    module
    name:    godot
    date:    14-Dec-2022
    version: 0.1.0
    author:  "Oldes"
    exports: [extract-gpck]
    needs:   [json]
    file:    https://raw.githubusercontent.com/Oldes/Rebol-Godot/master/godot.reb
    purpose: {
    	This script was maded only to examine files which may be found in an application made in Godot engine.

    	https://godotengine.org/
    	https://github.com/godotengine/godot
    }
    history: [
    	14-Dec-2022 "Oldes" {Initial version - useful to extract content of not encrypted Godot's *.pck files}
    ]
    license: MIT
]

system/options/log/godot: 3

decode-pck: function[file [port! file!]][
	src: either port? file [file][open/read/seek file]
	if #{47445043} <> read/part src 4 [
		sys/log/error 'GODOT "Not GPCK file!"
		return none
	]
	bin: binary read/part src 64000
	version: binary/read bin [UI32LE UI32LE UI32LE UI32LE]
	;? version
	binary/read bin [
		SKIP 64 ;; Reserved space
		files: UI32LE
	]
	;? files
	info: make map! files
	loop files [
		if 1000 > length? bin/buffer [
			append bin/buffer read/part src 32000
		]
		path: to string! trim/tail binary/read bin 'UI32LEBYTES
		info/:path: binary/read bin [
			offset: UI64LE
			size:   UI64LE
			md5:    BYTES 16
		]
	]
	info
]

decode-stex: function [
	{Extract content of the STEX file}
	data  [binary! file! url!]
	/only "Don't try to load an image, return raw binary instead"
][
	unless binary? data [ data: read data ]
	inp: binary data
	unless binary/read inp #{47445354} [return none]
	spec: binary/read inp [
		width:  UI32LE
		height: UI32LE
		type:   UI32LE  ;; 4 = RGBA?
		flags:  BYTES 4 ;UI32LE
	]
	num: binary/read inp 'UI32LE
	out: make block! num
	loop num [
		tmp:  binary/read inp 'UI32LEBYTES
		type: to string! take/part tmp 4
		;print [type spec] 
		append out tmp
	]
	unless only [ forall out [out/1: to image! out/1] ] 
	either num == 1 [first out][out]
]

decode-rsrc: function[
	{Extract content of the RSRC file}
	data  [binary! file! url!]
	/only "Don't try to load an image, return raw binary instead"
][
	unless binary? data [ data: read data ]
	inp: binary data
	unless binary/read inp #{52535243} [return none]
	spec: binary/read inp [
		UI32LE ;; endianess; 0 = LE, 1 = BE
		UI32LE ;; reserved
		UI32LE ;; MAJOR
		UI32LE ;; MINOR
		UI32LE ;; VERSION
	]
	if spec/1 <> 0 [
		sys/log/error 'GODOT "Big-endian RSRC file format not supported!"
		;@@ there should be support in the Bincode to set default endianess!
		return none
	]
	out: copy []
	class: to string! trim/tail binary/read inp 'UI32LEBYTES
	;? class
	append out class
	spec: binary/read inp [
		UI64LE ;; offset to metadata
		UI32LE ;; format flags
		UI64LE ;; UID

		SKIP 44 ;; 11xUI32LE
	]
	;? spec
	num: binary/read inp 'UI32LE ;; string table size
	strings: make block! num
	loop num [
		tmp: to string! trim/tail binary/read inp 'UI32LEBYTES
		try [tmp: to word! tmp]
		append strings tmp
	]
	;? strings
	num: binary/read inp 'UI32LE ;; amount of external resources
	;? num
	append/only out external: make block! num 
	loop num [
		append external binary/read inp [
			UI32LEBYTES  ;; class
			UI32LEBYTES  ;; path
			UI64LE       ;; ResourceUID
		]
	]
	num: binary/read inp 'UI32LE ;; amount of internal resources
	;? num
	append/only out internal: make block! num
	offsets: make block! 2 * num
	loop num [
		append append offsets
			to string! trim/tail binary/read inp 'UI32LEBYTES ;; local://...
			binary/read inp 'UI64LE ;; offset
	]
	;? offsets
	foreach [id ofs] offsets [
		binary/read inp [
			ATZ :ofs
			str: UI32LEBYTES
			num: UI32LE ;; properties
		]
		str: to string! trim/tail str
		;? str ? num
		loop num [
			binary/read inp [
				name: UI32LE ;; property name (index to the strings table)
				type: UI32LE
			]
			name: pickz strings name
			;? name
			;? type
			value: switch/default type [
				;VARIANT_NIL
				1  [ #[none] ]
				;VARIANT_BOOL
				2  [ 1 = binary/read inp 'UI32LE ]
				;VARIANT_INT
				3  [ binary/read inp 'UI32LE ]
				;VARIANT_FLOAT
				4  [ binary/read inp 'FLOAT ]
				;VARIANT_STRING
				5  [ to string! trim/tail binary/read inp 'UI32LEBYTES ]
				;VARIANT_VECTOR2
				10 [ as-pair binary/read inp [FLOAT FLOAT] ]
				;VARIANT_RECT2
				11 [ binary/read inp [FLOAT FLOAT FLOAT FLOAT] ]
				;VARIANT_VECTOR3
				12 [ binary/read inp [FLOAT FLOAT FLOAT] ]
				;VARIANT_PLANE
				13 [ binary/read inp [FLOAT FLOAT FLOAT FLOAT] ]
				;VARIANT_QUATERNION
				14 [ binary/read inp [FLOAT FLOAT FLOAT FLOAT] ]
				;VARIANT_AABB
				15 [ binary/read inp [FLOAT FLOAT FLOAT FLOAT FLOAT FLOAT] ]
				;VARIANT_BASIS
				16 [ binary/read inp [FLOAT FLOAT FLOAT FLOAT FLOAT FLOAT FLOAT FLOAT FLOAT] ]
				;VARIANT_TRANSFORM3D
				17 [ binary/read inp [FLOAT FLOAT FLOAT FLOAT FLOAT FLOAT FLOAT FLOAT FLOAT FLOAT FLOAT FLOAT] ]
				;VARIANT_TRANSFORM2D
				18 [ binary/read inp [FLOAT FLOAT FLOAT FLOAT FLOAT FLOAT] ]
				;VARIANT_COLOR
				20 [ binary/read inp [FLOAT FLOAT FLOAT FLOAT] ]
				;VARIANT_NODE_PATH
				22 [
					binary/read inp [names: UI16LE subnames: UI16LE]
					absolute: subnames & 32768 ;8000h
					subnames: subnames & 32767 ;7FFFh
					node: make block! 3
					append/only node tmp: make block! names
					loop names [
						append tmp to string! trim/tail binary/read inp 'UI32LEBYTES
					]
					append/only node tmp: make block! subnames
					loop subnames [
						append tmp to string! trim/tail binary/read inp 'UI32LEBYTES
					]
					head append node absolute
				]
				;VARIANT_RID
				23 [ binary/read inp 'UI32LE ]
				;VARIANT_OBJECT
				24 [
					reduce [
						type: binary/read inp 'UI32LE
						switch type [
							0 [ #[none] ] ;OBJECT_EMPTY
							1 [
								;OBJECT_EXTERNAL_RESOURCE
								reduce [
									to string! trim/tail binary/read inp 'UI32LEBYTES ;exttype
									to string! trim/tail binary/read inp 'UI32LEBYTES ;path
								]
							]
							2 [
								;OBJECT_INTERNAL_RESOURCE
								binary/read inp 'UI32LE ;index
							] 
							3 [
								;OBJECT_EXTERNAL_RESOURCE_INDEX
								binary/read inp 'UI32LE ;index
							] 
						]
					]
				]
				;@@VARIANT_INPUT_EVENT = 25,
				;@@VARIANT_DICTIONARY = 26,
				;@@VARIANT_ARRAY = 30,
				;VARIANT_PACKED_BYTE_ARRAY
				31 [
					second binary/read inp [len: UI32LE BYTES :len]
					;make vector! reduce ['uint8! tmp/2]
				]
				;VARIANT_PACKED_INT32_ARRAY
				32 [
					tmp: binary/read inp [len: UI32LE BYTES :len]
					make vector! reduce ['int32! tmp/2]
				]
				;VARIANT_PACKED_FLOAT32_ARRAY
				33 [
					tmp: binary/read inp [len: UI32LE BYTES :len]
					make vector! reduce ['float! tmp/2]
				]
				;@@VARIANT_PACKED_STRING_ARRAY = 34,
				;@@VARIANT_PACKED_VECTOR3_ARRAY = 35,
				;@@VARIANT_PACKED_COLOR_ARRAY = 36,
				;@@VARIANT_PACKED_VECTOR2_ARRAY = 37,	
				;VARIANT_INT64
				40 [ binary/read inp 'UI64LE ]
				;VARIANT_DOUBLE
				41 [ binary/read inp 'DOUBLE ]
				;VARIANT_CALLABLE = 42,
				;VARIANT_SIGNAL = 43,
				;VARIANT_STRING_NAME
				44 [ to string! trim/tail binary/read inp 'UI32LEBYTES ]
				;VARIANT_VECTOR2I
				45 [ binary/read inp [UI32LE UI32LE] ]
				;VARIANT_RECT2I
				46 [ binary/read inp [UI32LE UI32LE UI32LE UI32LE] ]
				;VARIANT_VECTOR3I
				47 [ binary/read inp [UI32LE UI32LE UI32LE] ]
				;VARIANT_PACKED_INT64_ARRAY
				48 [
					tmp: binary/read inp [len: UI32LE BYTES :len]
					make vector! reduce ['int64! tmp/2]
				]
				;VARIANT_PACKED_FLOAT64_ARRAY
				49 [
					tmp: binary/read inp [len: UI32LE BYTES :len]
					make vector! reduce ['decimal! tmp/2]
				]
				;VARIANT_VECTOR4
				50 [ binary/read inp [FLOAT FLOAT FLOAT FLOAT] ]
				;VARIANT_VECTOR4I
				51 [ binary/read inp [UI32LE UI32LE UI32LE UI32LE] ]
				;VARIANT_PROJECTION
				52 [ binary/read inp [FLOAT FLOAT FLOAT FLOAT FLOAT FLOAT FLOAT FLOAT FLOAT FLOAT FLOAT FLOAT FLOAT FLOAT FLOAT FLOAT] ]
			][
				sys/log/error 'GODOT ["Unsupported variant type:" as-red type]
				return none
			]
			;? value
			append append internal name value
		]
	]
	unless parse inp/buffer [any #"^@" #{52535243}][
		sys/log/error 'GODOT "RSRC magic at end not found!"
	]
	out
]

get-pck-file: func[
	"Resolves binary data from a GPCK scheme"
	port [port!] "GPCK scheme's context"
	file [any-string!]
	/check "Validate data's checksum"
	/local ctx bin inf
][
	all [
		ctx: port/state
		inf: pick ctx/info as string! :file
		bin: read/part skip head ctx/conn inf/1 inf/2
		any [not check inf/3 = checksum bin 'md5]
		bin
	]
]

sys/make-scheme [
	name: 'gpck
	actor: [
		open: func [port [port!] /local path bin][
			if port/state [
				cause-error 'access 'already-open port/spec/ref
			]
			port/state: context [
				conn:    none
				version: none
				files:   none
				info:    none
			]
			with port/state [
				try/except [
					conn: open/read/seek join port/spec/path port/spec/target
					if #{47445043} <> read/part conn 4 [
						cause-error 'access 'invalid-check port/spec/ref
					]
					bin: binary read/part conn 64000
					version: binary/read bin [UI32LE UI32LE UI32LE UI32LE]
					;? version
					binary/read bin [
						SKIP 64 ;; Reserved space
						files: UI32LE
					]
					? files
					info: make map! files
					loop files [
						if 1000 > length? bin/buffer [
							append bin/buffer read/part conn 32000
						]
						path: to string! trim/tail binary/read bin 'UI32LEBYTES
						parse path ["res://" (take/part path 6)] ;; don't use the res:// scheme
						;? path
						info/:path: binary/read bin [
							UI64LE   ;; offset
							UI64LE   ;; size
							BYTES 16 ;; MD5
						]
					]
				][
					cause-error 'access 'cannot-open reduce [port/spec/ref system/state/last-error/id]
				]
			]
			port
		]
		open?: func[port [port!]][ object? port/state ]
		read:  func[port [port!] /local ctx query info num out][
			unless port/state [open port]
			ctx: port/state
			? ctx
			if any-string? query: select port/spec 'query [
				return get-pck-file port query
			]
			num: length? ctx/info
			out: make block! num * 2
			foreach [file info] ctx/info [
				append append out as file! file read/part skip head ctx/conn info/1 info/2
			]
			out
		]
		pick:  func[port [port!] value [any-string!]][
			get-pck-file port :value
		]
		select: func[port [port!] value [any-string!]][

			get-pck-file port :value
		]
		close: func[port [port!]][
			try [close port/state/conn]
			port/state: none
			port
		]
		;query: func[port /mode field][	? field	]
		index?: func[port /local ctx][
			unless ctx: port/state [cause-error 'access 'not-open port/spec/ref]
			any [
				all [block? ctx/files index? ctx/files]
				1
			] 
		]
		length?: func[port /local ctx files ][
			unless ctx: port/state [cause-error 'access 'not-open port/spec/ref]
			files: ctx/files
			? files
			any [
				all [integer? files files]
				length? files
			] 
		]
	]
]

import-file: function[
	port [port!] "Opened GPCK port"
	spec [binary! string! file!] "Godot's *.import file"
	dir  [file!] "Target directory"
][
	if file? spec [spec: read/string spec]
	
	unless parse to string! spec [
		thru {^/importer="}          copy importer: to dbl-quote
		thru {^/path="res://}        copy res:      to dbl-quote
		thru {^/source_file="res://} copy file:     to dbl-quote
		to end
	] [
		sys/log/error 'GODOT "Failed to parse import spec!"
		return none
	]
	;? importer ? res ? file
	res: as file! res
	bin: get-pck-file port res
	file: join dir second split-path :file
	switch/default importer [
		"texture" [
			img: decode-stex/only bin ;; resolves just the raw binary
			case [
				binary? img [ write file img ]
				block?  img [
					sys/log/error 'GODOT "Multi-image not supported!" ? img
					ask "Press to continue..."
				]
				'else [
					sys/log/error 'GODOT ["Failed to decode stex file:" as-red res]
				]
			]
			return file
		]
		"wav" [
			try/except [
				rsrc: decode-rsrc bin
				bin: encode 'wav object [
					channels: either select rsrc/3 'stereo [2][1]
					data: :rsrc/3/data
				]
			][
				sys/log/error 'GODOT ["Failed to import wav resource:" as-red res]
			]
		]
		"clyde.dialogue" [
			try/except [
				rsrc: decode-rsrc bin
				bin: decode 'json :rsrc/3/__data__
			][
				sys/log/error 'GODOT ["Failed to import wav resource:" as-red res]
			]
		]
		"mp3" [] ;; do nothing
	][
		sys/log/error 'GODOT ["Unknown resource importer:" as-yellow importer]
	]
	write file bin
]


register-codec [
	name:  'stex
	type:  'image
	title: "Godot's texture file"
	suffixes: [%.stex]

	decode: :decode-stex
]

register-codec [
	name:  'gpck
	type:  'application
	title: "Godot's archive file"
	suffixes: [%.pck]

	decode: :decode-pck
]

extract-gpck: function[
	"Extract content of Godot's archive file"
	gpck [file!] "Path to Godot's *.pck file"
	/into        "Target directory"
	 dir [file!] "If not specified, current directory is used"
][
	port: open join gpck:// gpck
	target: any [dir %./]
	unless exists? dir [make-dir/deep dir]
	foreach [path info] port/state/info [
		;? path
		sys/log/info 'GODOT ["Extracting:" path]
		try/except [
			set [dir: file:] split-path path
			make-dir/deep target/:dir
			bin: get-pck-file port path
			write target/:path bin
			if %.import = suffix? file [
				import-file :port :bin target/:dir
			]
		][
			sys/log/error 'GODOT ["Failed to extract:" as-red path]
			sys/log/error 'GODOT system/state/last-error
		]
	]
]





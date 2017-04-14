pragma Ada_95;
pragma Warnings (Off);
with System;
package ada_main is

   gnat_argc : Integer;
   gnat_argv : System.Address;
   gnat_envp : System.Address;

   pragma Import (C, gnat_argc);
   pragma Import (C, gnat_argv);
   pragma Import (C, gnat_envp);

   gnat_exit_status : Integer;
   pragma Import (C, gnat_exit_status);

   GNAT_Version : constant String :=
                    "GNAT Version: GPL 2016 (20160515-49)" & ASCII.NUL;
   pragma Export (C, GNAT_Version, "__gnat_version");

   Ada_Main_Program_Name : constant String := "_ada_mc2dot" & ASCII.NUL;
   pragma Export (C, Ada_Main_Program_Name, "__gnat_ada_main_program_name");

   procedure adainit;
   pragma Export (C, adainit, "adainit");

   procedure adafinal;
   pragma Export (C, adafinal, "adafinal");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer;
   pragma Export (C, main, "main");

   type Version_32 is mod 2 ** 32;
   u00001 : constant Version_32 := 16#c4f59700#;
   pragma Export (C, u00001, "mc2dotB");
   u00002 : constant Version_32 := 16#b6df930e#;
   pragma Export (C, u00002, "system__standard_libraryB");
   u00003 : constant Version_32 := 16#337e9ce1#;
   pragma Export (C, u00003, "system__standard_libraryS");
   u00004 : constant Version_32 := 16#3ffc8e18#;
   pragma Export (C, u00004, "adaS");
   u00005 : constant Version_32 := 16#9094876d#;
   pragma Export (C, u00005, "ada__assertionsB");
   u00006 : constant Version_32 := 16#538f1e95#;
   pragma Export (C, u00006, "ada__assertionsS");
   u00007 : constant Version_32 := 16#472fa979#;
   pragma Export (C, u00007, "ada__exceptionsB");
   u00008 : constant Version_32 := 16#a2017425#;
   pragma Export (C, u00008, "ada__exceptionsS");
   u00009 : constant Version_32 := 16#e947e6a9#;
   pragma Export (C, u00009, "ada__exceptions__last_chance_handlerB");
   u00010 : constant Version_32 := 16#41e5552e#;
   pragma Export (C, u00010, "ada__exceptions__last_chance_handlerS");
   u00011 : constant Version_32 := 16#c3282aa7#;
   pragma Export (C, u00011, "systemS");
   u00012 : constant Version_32 := 16#5f84b5ab#;
   pragma Export (C, u00012, "system__soft_linksB");
   u00013 : constant Version_32 := 16#5dacf2f2#;
   pragma Export (C, u00013, "system__soft_linksS");
   u00014 : constant Version_32 := 16#b01dad17#;
   pragma Export (C, u00014, "system__parametersB");
   u00015 : constant Version_32 := 16#bd0227d8#;
   pragma Export (C, u00015, "system__parametersS");
   u00016 : constant Version_32 := 16#0f0cb66d#;
   pragma Export (C, u00016, "system__secondary_stackB");
   u00017 : constant Version_32 := 16#6849e5ce#;
   pragma Export (C, u00017, "system__secondary_stackS");
   u00018 : constant Version_32 := 16#39a03df9#;
   pragma Export (C, u00018, "system__storage_elementsB");
   u00019 : constant Version_32 := 16#eeeb60a3#;
   pragma Export (C, u00019, "system__storage_elementsS");
   u00020 : constant Version_32 := 16#41837d1e#;
   pragma Export (C, u00020, "system__stack_checkingB");
   u00021 : constant Version_32 := 16#4d97414f#;
   pragma Export (C, u00021, "system__stack_checkingS");
   u00022 : constant Version_32 := 16#87a448ff#;
   pragma Export (C, u00022, "system__exception_tableB");
   u00023 : constant Version_32 := 16#9e8643e5#;
   pragma Export (C, u00023, "system__exception_tableS");
   u00024 : constant Version_32 := 16#ce4af020#;
   pragma Export (C, u00024, "system__exceptionsB");
   u00025 : constant Version_32 := 16#ab4b4751#;
   pragma Export (C, u00025, "system__exceptionsS");
   u00026 : constant Version_32 := 16#4c9e814d#;
   pragma Export (C, u00026, "system__exceptions__machineS");
   u00027 : constant Version_32 := 16#aa0563fc#;
   pragma Export (C, u00027, "system__exceptions_debugB");
   u00028 : constant Version_32 := 16#bda2d363#;
   pragma Export (C, u00028, "system__exceptions_debugS");
   u00029 : constant Version_32 := 16#6c2f8802#;
   pragma Export (C, u00029, "system__img_intB");
   u00030 : constant Version_32 := 16#c1f3ca65#;
   pragma Export (C, u00030, "system__img_intS");
   u00031 : constant Version_32 := 16#39df8c17#;
   pragma Export (C, u00031, "system__tracebackB");
   u00032 : constant Version_32 := 16#9d0af463#;
   pragma Export (C, u00032, "system__tracebackS");
   u00033 : constant Version_32 := 16#9ed49525#;
   pragma Export (C, u00033, "system__traceback_entriesB");
   u00034 : constant Version_32 := 16#c373dcd7#;
   pragma Export (C, u00034, "system__traceback_entriesS");
   u00035 : constant Version_32 := 16#6fd210f2#;
   pragma Export (C, u00035, "system__traceback__symbolicB");
   u00036 : constant Version_32 := 16#dd19f67a#;
   pragma Export (C, u00036, "system__traceback__symbolicS");
   u00037 : constant Version_32 := 16#701f9d88#;
   pragma Export (C, u00037, "ada__exceptions__tracebackB");
   u00038 : constant Version_32 := 16#20245e75#;
   pragma Export (C, u00038, "ada__exceptions__tracebackS");
   u00039 : constant Version_32 := 16#57a37a42#;
   pragma Export (C, u00039, "system__address_imageB");
   u00040 : constant Version_32 := 16#62c4b79d#;
   pragma Export (C, u00040, "system__address_imageS");
   u00041 : constant Version_32 := 16#8c33a517#;
   pragma Export (C, u00041, "system__wch_conB");
   u00042 : constant Version_32 := 16#d8550875#;
   pragma Export (C, u00042, "system__wch_conS");
   u00043 : constant Version_32 := 16#9721e840#;
   pragma Export (C, u00043, "system__wch_stwB");
   u00044 : constant Version_32 := 16#f5442474#;
   pragma Export (C, u00044, "system__wch_stwS");
   u00045 : constant Version_32 := 16#a831679c#;
   pragma Export (C, u00045, "system__wch_cnvB");
   u00046 : constant Version_32 := 16#d7e2b286#;
   pragma Export (C, u00046, "system__wch_cnvS");
   u00047 : constant Version_32 := 16#5ab55268#;
   pragma Export (C, u00047, "interfacesS");
   u00048 : constant Version_32 := 16#ece6fdb6#;
   pragma Export (C, u00048, "system__wch_jisB");
   u00049 : constant Version_32 := 16#5792aba7#;
   pragma Export (C, u00049, "system__wch_jisS");
   u00050 : constant Version_32 := 16#52f1910f#;
   pragma Export (C, u00050, "system__assertionsB");
   u00051 : constant Version_32 := 16#0ea50633#;
   pragma Export (C, u00051, "system__assertionsS");
   u00052 : constant Version_32 := 16#87cd2ab9#;
   pragma Export (C, u00052, "ada__calendar__delaysB");
   u00053 : constant Version_32 := 16#b27fb9e9#;
   pragma Export (C, u00053, "ada__calendar__delaysS");
   u00054 : constant Version_32 := 16#c5dcd3d2#;
   pragma Export (C, u00054, "ada__calendarB");
   u00055 : constant Version_32 := 16#12a38fcc#;
   pragma Export (C, u00055, "ada__calendarS");
   u00056 : constant Version_32 := 16#769e25e6#;
   pragma Export (C, u00056, "interfaces__cB");
   u00057 : constant Version_32 := 16#70be4e8c#;
   pragma Export (C, u00057, "interfaces__cS");
   u00058 : constant Version_32 := 16#a6535153#;
   pragma Export (C, u00058, "system__os_primitivesB");
   u00059 : constant Version_32 := 16#49a73bd1#;
   pragma Export (C, u00059, "system__os_primitivesS");
   u00060 : constant Version_32 := 16#ee80728a#;
   pragma Export (C, u00060, "system__tracesB");
   u00061 : constant Version_32 := 16#3135420d#;
   pragma Export (C, u00061, "system__tracesS");
   u00062 : constant Version_32 := 16#8f637df8#;
   pragma Export (C, u00062, "ada__characters__handlingB");
   u00063 : constant Version_32 := 16#3b3f6154#;
   pragma Export (C, u00063, "ada__characters__handlingS");
   u00064 : constant Version_32 := 16#12c24a43#;
   pragma Export (C, u00064, "ada__charactersS");
   u00065 : constant Version_32 := 16#4b7bb96a#;
   pragma Export (C, u00065, "ada__characters__latin_1S");
   u00066 : constant Version_32 := 16#af50e98f#;
   pragma Export (C, u00066, "ada__stringsS");
   u00067 : constant Version_32 := 16#e2ea8656#;
   pragma Export (C, u00067, "ada__strings__mapsB");
   u00068 : constant Version_32 := 16#1e526bec#;
   pragma Export (C, u00068, "ada__strings__mapsS");
   u00069 : constant Version_32 := 16#a4e2d63b#;
   pragma Export (C, u00069, "system__bit_opsB");
   u00070 : constant Version_32 := 16#0765e3a3#;
   pragma Export (C, u00070, "system__bit_opsS");
   u00071 : constant Version_32 := 16#f7ae5624#;
   pragma Export (C, u00071, "system__unsigned_typesS");
   u00072 : constant Version_32 := 16#92f05f13#;
   pragma Export (C, u00072, "ada__strings__maps__constantsS");
   u00073 : constant Version_32 := 16#451bdd8a#;
   pragma Export (C, u00073, "ada__command_lineB");
   u00074 : constant Version_32 := 16#d59e21a4#;
   pragma Export (C, u00074, "ada__command_lineS");
   u00075 : constant Version_32 := 16#5e196e91#;
   pragma Export (C, u00075, "ada__containersS");
   u00076 : constant Version_32 := 16#1b0ca7f2#;
   pragma Export (C, u00076, "ada__directoriesB");
   u00077 : constant Version_32 := 16#eb9f206b#;
   pragma Export (C, u00077, "ada__directoriesS");
   u00078 : constant Version_32 := 16#8f218b8f#;
   pragma Export (C, u00078, "ada__calendar__formattingB");
   u00079 : constant Version_32 := 16#67ade573#;
   pragma Export (C, u00079, "ada__calendar__formattingS");
   u00080 : constant Version_32 := 16#e3cca715#;
   pragma Export (C, u00080, "ada__calendar__time_zonesB");
   u00081 : constant Version_32 := 16#6dc27f8f#;
   pragma Export (C, u00081, "ada__calendar__time_zonesS");
   u00082 : constant Version_32 := 16#d763507a#;
   pragma Export (C, u00082, "system__val_intB");
   u00083 : constant Version_32 := 16#8b8d0098#;
   pragma Export (C, u00083, "system__val_intS");
   u00084 : constant Version_32 := 16#1d9142a4#;
   pragma Export (C, u00084, "system__val_unsB");
   u00085 : constant Version_32 := 16#e706bb1f#;
   pragma Export (C, u00085, "system__val_unsS");
   u00086 : constant Version_32 := 16#27b600b2#;
   pragma Export (C, u00086, "system__val_utilB");
   u00087 : constant Version_32 := 16#6f889c59#;
   pragma Export (C, u00087, "system__val_utilS");
   u00088 : constant Version_32 := 16#d1060688#;
   pragma Export (C, u00088, "system__case_utilB");
   u00089 : constant Version_32 := 16#e7214370#;
   pragma Export (C, u00089, "system__case_utilS");
   u00090 : constant Version_32 := 16#faa9a7b2#;
   pragma Export (C, u00090, "system__val_realB");
   u00091 : constant Version_32 := 16#3d015db6#;
   pragma Export (C, u00091, "system__val_realS");
   u00092 : constant Version_32 := 16#6c05c057#;
   pragma Export (C, u00092, "system__exn_llfB");
   u00093 : constant Version_32 := 16#7f56917b#;
   pragma Export (C, u00093, "system__exn_llfS");
   u00094 : constant Version_32 := 16#1b28662b#;
   pragma Export (C, u00094, "system__float_controlB");
   u00095 : constant Version_32 := 16#23d4699b#;
   pragma Export (C, u00095, "system__float_controlS");
   u00096 : constant Version_32 := 16#93584cd0#;
   pragma Export (C, u00096, "system__powten_tableS");
   u00097 : constant Version_32 := 16#ab4ad33a#;
   pragma Export (C, u00097, "ada__directories__validityB");
   u00098 : constant Version_32 := 16#d34bdf62#;
   pragma Export (C, u00098, "ada__directories__validityS");
   u00099 : constant Version_32 := 16#e5480ede#;
   pragma Export (C, u00099, "ada__strings__fixedB");
   u00100 : constant Version_32 := 16#a86b22b3#;
   pragma Export (C, u00100, "ada__strings__fixedS");
   u00101 : constant Version_32 := 16#e5c7cf31#;
   pragma Export (C, u00101, "ada__strings__searchB");
   u00102 : constant Version_32 := 16#c1ab8667#;
   pragma Export (C, u00102, "ada__strings__searchS");
   u00103 : constant Version_32 := 16#5130abd7#;
   pragma Export (C, u00103, "ada__strings__unboundedB");
   u00104 : constant Version_32 := 16#4c956ffe#;
   pragma Export (C, u00104, "ada__strings__unboundedS");
   u00105 : constant Version_32 := 16#920eada5#;
   pragma Export (C, u00105, "ada__tagsB");
   u00106 : constant Version_32 := 16#13ca27f3#;
   pragma Export (C, u00106, "ada__tagsS");
   u00107 : constant Version_32 := 16#c3335bfd#;
   pragma Export (C, u00107, "system__htableB");
   u00108 : constant Version_32 := 16#47ea994d#;
   pragma Export (C, u00108, "system__htableS");
   u00109 : constant Version_32 := 16#089f5cd0#;
   pragma Export (C, u00109, "system__string_hashB");
   u00110 : constant Version_32 := 16#e5b4f233#;
   pragma Export (C, u00110, "system__string_hashS");
   u00111 : constant Version_32 := 16#afdbf393#;
   pragma Export (C, u00111, "system__val_lluB");
   u00112 : constant Version_32 := 16#8d5c0156#;
   pragma Export (C, u00112, "system__val_lluS");
   u00113 : constant Version_32 := 16#5b9edcc4#;
   pragma Export (C, u00113, "system__compare_array_unsigned_8B");
   u00114 : constant Version_32 := 16#6a2b5b2a#;
   pragma Export (C, u00114, "system__compare_array_unsigned_8S");
   u00115 : constant Version_32 := 16#5f72f755#;
   pragma Export (C, u00115, "system__address_operationsB");
   u00116 : constant Version_32 := 16#d0249494#;
   pragma Export (C, u00116, "system__address_operationsS");
   u00117 : constant Version_32 := 16#6a86c9a5#;
   pragma Export (C, u00117, "system__storage_pools__subpoolsB");
   u00118 : constant Version_32 := 16#cc5a1856#;
   pragma Export (C, u00118, "system__storage_pools__subpoolsS");
   u00119 : constant Version_32 := 16#6abe5dbe#;
   pragma Export (C, u00119, "system__finalization_mastersB");
   u00120 : constant Version_32 := 16#98d4136d#;
   pragma Export (C, u00120, "system__finalization_mastersS");
   u00121 : constant Version_32 := 16#7268f812#;
   pragma Export (C, u00121, "system__img_boolB");
   u00122 : constant Version_32 := 16#36f15b4c#;
   pragma Export (C, u00122, "system__img_boolS");
   u00123 : constant Version_32 := 16#d7aac20c#;
   pragma Export (C, u00123, "system__ioB");
   u00124 : constant Version_32 := 16#5d6adde8#;
   pragma Export (C, u00124, "system__ioS");
   u00125 : constant Version_32 := 16#cf417de3#;
   pragma Export (C, u00125, "ada__finalizationS");
   u00126 : constant Version_32 := 16#10558b11#;
   pragma Export (C, u00126, "ada__streamsB");
   u00127 : constant Version_32 := 16#2e6701ab#;
   pragma Export (C, u00127, "ada__streamsS");
   u00128 : constant Version_32 := 16#db5c917c#;
   pragma Export (C, u00128, "ada__io_exceptionsS");
   u00129 : constant Version_32 := 16#95817ed8#;
   pragma Export (C, u00129, "system__finalization_rootB");
   u00130 : constant Version_32 := 16#8cda5937#;
   pragma Export (C, u00130, "system__finalization_rootS");
   u00131 : constant Version_32 := 16#6d4d969a#;
   pragma Export (C, u00131, "system__storage_poolsB");
   u00132 : constant Version_32 := 16#e0c5b40a#;
   pragma Export (C, u00132, "system__storage_poolsS");
   u00133 : constant Version_32 := 16#9aad1ff1#;
   pragma Export (C, u00133, "system__storage_pools__subpools__finalizationB");
   u00134 : constant Version_32 := 16#fe2f4b3a#;
   pragma Export (C, u00134, "system__storage_pools__subpools__finalizationS");
   u00135 : constant Version_32 := 16#020a3f4d#;
   pragma Export (C, u00135, "system__atomic_countersB");
   u00136 : constant Version_32 := 16#7774072a#;
   pragma Export (C, u00136, "system__atomic_countersS");
   u00137 : constant Version_32 := 16#f4e1c091#;
   pragma Export (C, u00137, "system__stream_attributesB");
   u00138 : constant Version_32 := 16#8bc30a4e#;
   pragma Export (C, u00138, "system__stream_attributesS");
   u00139 : constant Version_32 := 16#b3b9fca9#;
   pragma Export (C, u00139, "system__crtlS");
   u00140 : constant Version_32 := 16#d761112b#;
   pragma Export (C, u00140, "system__file_attributesS");
   u00141 : constant Version_32 := 16#acefa820#;
   pragma Export (C, u00141, "system__os_constantsS");
   u00142 : constant Version_32 := 16#b29d05bd#;
   pragma Export (C, u00142, "system__file_ioB");
   u00143 : constant Version_32 := 16#6459cbc2#;
   pragma Export (C, u00143, "system__file_ioS");
   u00144 : constant Version_32 := 16#84a27f0d#;
   pragma Export (C, u00144, "interfaces__c_streamsB");
   u00145 : constant Version_32 := 16#b1330297#;
   pragma Export (C, u00145, "interfaces__c_streamsS");
   u00146 : constant Version_32 := 16#7358ec0a#;
   pragma Export (C, u00146, "system__os_libB");
   u00147 : constant Version_32 := 16#bf5ce13f#;
   pragma Export (C, u00147, "system__os_libS");
   u00148 : constant Version_32 := 16#1a817b8e#;
   pragma Export (C, u00148, "system__stringsB");
   u00149 : constant Version_32 := 16#bd973bc1#;
   pragma Export (C, u00149, "system__stringsS");
   u00150 : constant Version_32 := 16#3eb7b00f#;
   pragma Export (C, u00150, "system__file_control_blockS");
   u00151 : constant Version_32 := 16#933fac2f#;
   pragma Export (C, u00151, "system__regexpB");
   u00152 : constant Version_32 := 16#e01a8d6b#;
   pragma Export (C, u00152, "system__regexpS");
   u00153 : constant Version_32 := 16#9fad3aa0#;
   pragma Export (C, u00153, "ada__real_timeB");
   u00154 : constant Version_32 := 16#8a504209#;
   pragma Export (C, u00154, "ada__real_timeS");
   u00155 : constant Version_32 := 16#a540e70d#;
   pragma Export (C, u00155, "system__taskingB");
   u00156 : constant Version_32 := 16#2410879e#;
   pragma Export (C, u00156, "system__taskingS");
   u00157 : constant Version_32 := 16#ae36db3a#;
   pragma Export (C, u00157, "system__task_primitivesS");
   u00158 : constant Version_32 := 16#eba442ab#;
   pragma Export (C, u00158, "system__os_interfaceB");
   u00159 : constant Version_32 := 16#225a1d94#;
   pragma Export (C, u00159, "system__os_interfaceS");
   u00160 : constant Version_32 := 16#24379d76#;
   pragma Export (C, u00160, "interfaces__c__extensionsS");
   u00161 : constant Version_32 := 16#738dd9f2#;
   pragma Export (C, u00161, "system__task_primitives__operationsB");
   u00162 : constant Version_32 := 16#0980a7f3#;
   pragma Export (C, u00162, "system__task_primitives__operationsS");
   u00163 : constant Version_32 := 16#89b55e64#;
   pragma Export (C, u00163, "system__interrupt_managementB");
   u00164 : constant Version_32 := 16#5e06908e#;
   pragma Export (C, u00164, "system__interrupt_managementS");
   u00165 : constant Version_32 := 16#f65595cf#;
   pragma Export (C, u00165, "system__multiprocessorsB");
   u00166 : constant Version_32 := 16#fb84b5d4#;
   pragma Export (C, u00166, "system__multiprocessorsS");
   u00167 : constant Version_32 := 16#e0fce7f8#;
   pragma Export (C, u00167, "system__task_infoB");
   u00168 : constant Version_32 := 16#433297a6#;
   pragma Export (C, u00168, "system__task_infoS");
   u00169 : constant Version_32 := 16#85267e7e#;
   pragma Export (C, u00169, "system__tasking__debugB");
   u00170 : constant Version_32 := 16#511cd042#;
   pragma Export (C, u00170, "system__tasking__debugS");
   u00171 : constant Version_32 := 16#fd83e873#;
   pragma Export (C, u00171, "system__concat_2B");
   u00172 : constant Version_32 := 16#c188fd77#;
   pragma Export (C, u00172, "system__concat_2S");
   u00173 : constant Version_32 := 16#2b70b149#;
   pragma Export (C, u00173, "system__concat_3B");
   u00174 : constant Version_32 := 16#c8587602#;
   pragma Export (C, u00174, "system__concat_3S");
   u00175 : constant Version_32 := 16#d0432c8d#;
   pragma Export (C, u00175, "system__img_enum_newB");
   u00176 : constant Version_32 := 16#a2642c67#;
   pragma Export (C, u00176, "system__img_enum_newS");
   u00177 : constant Version_32 := 16#9dca6636#;
   pragma Export (C, u00177, "system__img_lliB");
   u00178 : constant Version_32 := 16#d2677f76#;
   pragma Export (C, u00178, "system__img_lliS");
   u00179 : constant Version_32 := 16#118e865d#;
   pragma Export (C, u00179, "system__stack_usageB");
   u00180 : constant Version_32 := 16#3a3ac346#;
   pragma Export (C, u00180, "system__stack_usageS");
   u00181 : constant Version_32 := 16#75de1dee#;
   pragma Export (C, u00181, "ada__strings__hashB");
   u00182 : constant Version_32 := 16#3655ad4c#;
   pragma Export (C, u00182, "ada__strings__hashS");
   u00183 : constant Version_32 := 16#d5bfa9f3#;
   pragma Export (C, u00183, "ada__text_ioB");
   u00184 : constant Version_32 := 16#2d7da68a#;
   pragma Export (C, u00184, "ada__text_ioS");
   u00185 : constant Version_32 := 16#7061ba7e#;
   pragma Export (C, u00185, "bool_dyn_storeB");
   u00186 : constant Version_32 := 16#8a8bb1a3#;
   pragma Export (C, u00186, "bool_dyn_storeS");
   u00187 : constant Version_32 := 16#4f0e00d0#;
   pragma Export (C, u00187, "configurationsB");
   u00188 : constant Version_32 := 16#73fa3501#;
   pragma Export (C, u00188, "configurationsS");
   u00189 : constant Version_32 := 16#a2e70198#;
   pragma Export (C, u00189, "dyn_storeB");
   u00190 : constant Version_32 := 16#9b20c5ec#;
   pragma Export (C, u00190, "dyn_storeS");
   u00191 : constant Version_32 := 16#ae082045#;
   pragma Export (C, u00191, "flagsS");
   u00192 : constant Version_32 := 16#802e16bb#;
   pragma Export (C, u00192, "my_atomic_countersB");
   u00193 : constant Version_32 := 16#926c3369#;
   pragma Export (C, u00193, "my_atomic_countersS");
   u00194 : constant Version_32 := 16#a3f51ba9#;
   pragma Export (C, u00194, "spin_locksB");
   u00195 : constant Version_32 := 16#eac90723#;
   pragma Export (C, u00195, "spin_locksS");
   u00196 : constant Version_32 := 16#cc39f920#;
   pragma Export (C, u00196, "system__atomic_primitivesB");
   u00197 : constant Version_32 := 16#08a65d51#;
   pragma Export (C, u00197, "system__atomic_primitivesS");
   u00198 : constant Version_32 := 16#fd2ad2f1#;
   pragma Export (C, u00198, "gnatS");
   u00199 : constant Version_32 := 16#6ce04563#;
   pragma Export (C, u00199, "gnat__directory_operationsB");
   u00200 : constant Version_32 := 16#42b5cd24#;
   pragma Export (C, u00200, "gnat__directory_operationsS");
   u00201 : constant Version_32 := 16#c024395a#;
   pragma Export (C, u00201, "gnat__os_libS");
   u00202 : constant Version_32 := 16#808cfd38#;
   pragma Export (C, u00202, "nicknamesB");
   u00203 : constant Version_32 := 16#10add054#;
   pragma Export (C, u00203, "nicknamesS");
   u00204 : constant Version_32 := 16#7dbbd31d#;
   pragma Export (C, u00204, "text_ioS");
   u00205 : constant Version_32 := 16#e5386b37#;
   pragma Export (C, u00205, "dotlibB");
   u00206 : constant Version_32 := 16#04accd35#;
   pragma Export (C, u00206, "dotlibS");
   u00207 : constant Version_32 := 16#5a895de2#;
   pragma Export (C, u00207, "system__pool_globalB");
   u00208 : constant Version_32 := 16#7141203e#;
   pragma Export (C, u00208, "system__pool_globalS");
   u00209 : constant Version_32 := 16#a6359005#;
   pragma Export (C, u00209, "system__memoryB");
   u00210 : constant Version_32 := 16#9a554c93#;
   pragma Export (C, u00210, "system__memoryS");
   u00211 : constant Version_32 := 16#fc9da4b5#;
   pragma Export (C, u00211, "system__tasking__stagesB");
   u00212 : constant Version_32 := 16#21a9077d#;
   pragma Export (C, u00212, "system__tasking__stagesS");
   u00213 : constant Version_32 := 16#100eaf58#;
   pragma Export (C, u00213, "system__restrictionsB");
   u00214 : constant Version_32 := 16#6a886006#;
   pragma Export (C, u00214, "system__restrictionsS");
   u00215 : constant Version_32 := 16#71e7b7a1#;
   pragma Export (C, u00215, "system__tasking__initializationB");
   u00216 : constant Version_32 := 16#ed62fcff#;
   pragma Export (C, u00216, "system__tasking__initializationS");
   u00217 : constant Version_32 := 16#eeadc70a#;
   pragma Export (C, u00217, "system__soft_links__taskingB");
   u00218 : constant Version_32 := 16#5ae92880#;
   pragma Export (C, u00218, "system__soft_links__taskingS");
   u00219 : constant Version_32 := 16#17d21067#;
   pragma Export (C, u00219, "ada__exceptions__is_null_occurrenceB");
   u00220 : constant Version_32 := 16#e1d7566f#;
   pragma Export (C, u00220, "ada__exceptions__is_null_occurrenceS");
   u00221 : constant Version_32 := 16#7995e1aa#;
   pragma Export (C, u00221, "system__tasking__task_attributesB");
   u00222 : constant Version_32 := 16#a1da3c09#;
   pragma Export (C, u00222, "system__tasking__task_attributesS");
   u00223 : constant Version_32 := 16#35ce8314#;
   pragma Export (C, u00223, "system__tasking__queuingB");
   u00224 : constant Version_32 := 16#05e644a6#;
   pragma Export (C, u00224, "system__tasking__queuingS");
   u00225 : constant Version_32 := 16#f83990e5#;
   pragma Export (C, u00225, "system__tasking__protected_objectsB");
   u00226 : constant Version_32 := 16#5744f344#;
   pragma Export (C, u00226, "system__tasking__protected_objectsS");
   u00227 : constant Version_32 := 16#9fa349e0#;
   pragma Export (C, u00227, "system__tasking__protected_objects__entriesB");
   u00228 : constant Version_32 := 16#a0c7bfc6#;
   pragma Export (C, u00228, "system__tasking__protected_objects__entriesS");
   u00229 : constant Version_32 := 16#9dcd4743#;
   pragma Export (C, u00229, "system__tasking__rendezvousB");
   u00230 : constant Version_32 := 16#3e44c873#;
   pragma Export (C, u00230, "system__tasking__rendezvousS");
   u00231 : constant Version_32 := 16#d3d9b1ce#;
   pragma Export (C, u00231, "system__tasking__entry_callsB");
   u00232 : constant Version_32 := 16#ddf2aa0b#;
   pragma Export (C, u00232, "system__tasking__entry_callsS");
   u00233 : constant Version_32 := 16#ce83633b#;
   pragma Export (C, u00233, "system__tasking__protected_objects__operationsB");
   u00234 : constant Version_32 := 16#902e29cd#;
   pragma Export (C, u00234, "system__tasking__protected_objects__operationsS");
   u00235 : constant Version_32 := 16#67e431ef#;
   pragma Export (C, u00235, "system__tasking__utilitiesB");
   u00236 : constant Version_32 := 16#51068caf#;
   pragma Export (C, u00236, "system__tasking__utilitiesS");
   u00237 : constant Version_32 := 16#bd6fc52e#;
   pragma Export (C, u00237, "system__traces__taskingB");
   u00238 : constant Version_32 := 16#0b40d4b2#;
   pragma Export (C, u00238, "system__traces__taskingS");
   --  BEGIN ELABORATION ORDER
   --  ada%s
   --  ada.characters%s
   --  ada.characters.handling%s
   --  ada.characters.latin_1%s
   --  ada.command_line%s
   --  gnat%s
   --  interfaces%s
   --  system%s
   --  system.address_operations%s
   --  system.address_operations%b
   --  system.atomic_counters%s
   --  system.atomic_counters%b
   --  system.atomic_primitives%s
   --  system.atomic_primitives%b
   --  system.case_util%s
   --  system.case_util%b
   --  system.exn_llf%s
   --  system.exn_llf%b
   --  system.float_control%s
   --  system.float_control%b
   --  system.htable%s
   --  system.img_bool%s
   --  system.img_bool%b
   --  system.img_enum_new%s
   --  system.img_enum_new%b
   --  system.img_int%s
   --  system.img_int%b
   --  system.img_lli%s
   --  system.img_lli%b
   --  system.io%s
   --  system.io%b
   --  system.multiprocessors%s
   --  system.os_primitives%s
   --  system.os_primitives%b
   --  system.parameters%s
   --  system.parameters%b
   --  system.crtl%s
   --  interfaces.c_streams%s
   --  interfaces.c_streams%b
   --  system.powten_table%s
   --  system.restrictions%s
   --  system.restrictions%b
   --  system.standard_library%s
   --  system.exceptions_debug%s
   --  system.exceptions_debug%b
   --  system.storage_elements%s
   --  system.storage_elements%b
   --  system.stack_checking%s
   --  system.stack_checking%b
   --  system.stack_usage%s
   --  system.stack_usage%b
   --  system.string_hash%s
   --  system.string_hash%b
   --  system.htable%b
   --  system.strings%s
   --  system.strings%b
   --  system.os_lib%s
   --  gnat.os_lib%s
   --  system.task_info%s
   --  system.task_info%b
   --  system.traceback_entries%s
   --  system.traceback_entries%b
   --  ada.exceptions%s
   --  ada.exceptions.is_null_occurrence%s
   --  ada.exceptions.is_null_occurrence%b
   --  system.soft_links%s
   --  system.traces%s
   --  system.traces%b
   --  system.unsigned_types%s
   --  system.val_int%s
   --  system.val_llu%s
   --  system.val_real%s
   --  system.val_uns%s
   --  system.val_util%s
   --  system.val_util%b
   --  system.val_uns%b
   --  system.val_real%b
   --  system.val_llu%b
   --  system.val_int%b
   --  system.wch_con%s
   --  system.wch_con%b
   --  system.wch_cnv%s
   --  system.wch_jis%s
   --  system.wch_jis%b
   --  system.wch_cnv%b
   --  system.wch_stw%s
   --  system.wch_stw%b
   --  ada.exceptions.last_chance_handler%s
   --  ada.exceptions.last_chance_handler%b
   --  ada.exceptions.traceback%s
   --  system.address_image%s
   --  system.bit_ops%s
   --  system.bit_ops%b
   --  system.compare_array_unsigned_8%s
   --  system.compare_array_unsigned_8%b
   --  system.concat_2%s
   --  system.concat_2%b
   --  system.concat_3%s
   --  system.concat_3%b
   --  system.exception_table%s
   --  system.exception_table%b
   --  ada.containers%s
   --  ada.io_exceptions%s
   --  ada.strings%s
   --  ada.strings.hash%s
   --  ada.strings.hash%b
   --  ada.strings.maps%s
   --  ada.strings.fixed%s
   --  ada.strings.maps.constants%s
   --  ada.strings.search%s
   --  ada.strings.search%b
   --  ada.tags%s
   --  ada.streams%s
   --  ada.streams%b
   --  interfaces.c%s
   --  system.multiprocessors%b
   --  interfaces.c.extensions%s
   --  system.exceptions%s
   --  system.exceptions%b
   --  system.exceptions.machine%s
   --  system.file_control_block%s
   --  system.file_io%s
   --  system.finalization_root%s
   --  system.finalization_root%b
   --  ada.finalization%s
   --  system.os_constants%s
   --  system.os_interface%s
   --  system.os_interface%b
   --  system.interrupt_management%s
   --  system.interrupt_management%b
   --  system.storage_pools%s
   --  system.storage_pools%b
   --  system.finalization_masters%s
   --  system.storage_pools.subpools%s
   --  system.storage_pools.subpools.finalization%s
   --  system.storage_pools.subpools.finalization%b
   --  system.stream_attributes%s
   --  system.stream_attributes%b
   --  system.task_primitives%s
   --  system.tasking%s
   --  system.task_primitives.operations%s
   --  system.tasking%b
   --  system.tasking.debug%s
   --  system.tasking.debug%b
   --  system.task_primitives.operations%b
   --  system.traces.tasking%s
   --  system.traces.tasking%b
   --  ada.calendar%s
   --  ada.calendar%b
   --  ada.calendar.delays%s
   --  ada.calendar.delays%b
   --  ada.calendar.time_zones%s
   --  ada.calendar.time_zones%b
   --  ada.calendar.formatting%s
   --  gnat.directory_operations%s
   --  system.assertions%s
   --  system.assertions%b
   --  ada.assertions%s
   --  ada.assertions%b
   --  system.file_attributes%s
   --  system.memory%s
   --  system.memory%b
   --  system.standard_library%b
   --  system.pool_global%s
   --  system.pool_global%b
   --  system.secondary_stack%s
   --  system.storage_pools.subpools%b
   --  system.finalization_masters%b
   --  system.file_io%b
   --  interfaces.c%b
   --  ada.tags%b
   --  ada.strings.fixed%b
   --  ada.strings.maps%b
   --  system.soft_links%b
   --  system.os_lib%b
   --  ada.command_line%b
   --  ada.characters.handling%b
   --  system.secondary_stack%b
   --  gnat.directory_operations%b
   --  ada.calendar.formatting%b
   --  system.address_image%b
   --  ada.exceptions.traceback%b
   --  ada.strings.unbounded%s
   --  ada.strings.unbounded%b
   --  ada.directories%s
   --  ada.directories.validity%s
   --  ada.directories.validity%b
   --  system.regexp%s
   --  system.regexp%b
   --  ada.directories%b
   --  system.soft_links.tasking%s
   --  system.soft_links.tasking%b
   --  system.tasking.entry_calls%s
   --  system.tasking.initialization%s
   --  system.tasking.task_attributes%s
   --  system.tasking.task_attributes%b
   --  system.tasking.utilities%s
   --  system.traceback%s
   --  system.traceback%b
   --  system.traceback.symbolic%s
   --  system.traceback.symbolic%b
   --  ada.exceptions%b
   --  system.tasking.initialization%b
   --  ada.real_time%s
   --  ada.real_time%b
   --  ada.text_io%s
   --  ada.text_io%b
   --  system.tasking.protected_objects%s
   --  system.tasking.protected_objects%b
   --  system.tasking.protected_objects.entries%s
   --  system.tasking.protected_objects.entries%b
   --  system.tasking.queuing%s
   --  system.tasking.queuing%b
   --  system.tasking.utilities%b
   --  system.tasking.rendezvous%s
   --  system.tasking.protected_objects.operations%s
   --  system.tasking.protected_objects.operations%b
   --  system.tasking.rendezvous%b
   --  system.tasking.entry_calls%b
   --  system.tasking.stages%s
   --  system.tasking.stages%b
   --  text_io%s
   --  spin_locks%s
   --  spin_locks%b
   --  bool_dyn_store%s
   --  bool_dyn_store%b
   --  dotlib%s
   --  dyn_store%s
   --  flags%s
   --  dotlib%b
   --  configurations%s
   --  my_atomic_counters%s
   --  my_atomic_counters%b
   --  dyn_store%b
   --  nicknames%s
   --  nicknames%b
   --  configurations%b
   --  mc2dot%b
   --  END ELABORATION ORDER


end ada_main;

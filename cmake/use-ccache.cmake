option(USE_CCACHE "Use ccache if available" ON)

find_program(CCACHE_PROGRAM ccache)

if (USE_CCACHE AND CCACHE_PROGRAM)
    set_property(GLOBAL PROPERTY RULE_LAUNCH_COMPILE "${CCACHE_PROGRAM}")
endif ()
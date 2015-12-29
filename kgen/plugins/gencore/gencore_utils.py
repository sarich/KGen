# gen_core_utils.py

# NOTES for supporting linked list
# STATE part
# A) save address of all saved derived type variables
# B) when a a pointer derived type is being saved, first check if the address of the target
#    variable is in the address list
#    a) if exits, do not save and leave a mark
#    b) if not exists, save and put the address of the saved variable in the address list
#
# KERNEL part
# A) when a pointer derived type is being read, first check if a mark exists,
#   a) If not exists, read and save the address of the derived type variable in dtype list with a mark
#   b) If exists, do not read and associate to a derived type variable in the dtype list according to the mark
#
# Mark
# A) derived type id
# B) IO id
#
# Address list
# A) an array of integers representing addresss
# B) generate in (named?) data block using common block
# C) external functions for using the address list
#
# Dtype list
# A) Two separate one dimensional singly linked list: one for dervied type id and the other for IO id
# B) May use hash integer for derived type id
# C) IO id will be saved in each kw_ kr_ subroutine with SAVE attribute
# D) generate in (named?) data block
# E) Use module for dtype list and manipulate functions. Use Interface for assigning pointer

import statements
import block_statements

shared_objects = {}
shared_objects['driver_object'] = None

state_gencore_parts = {}
state_gencore_contains = []

kernel_gencore_parts = {}
kernel_gencore_contains = []

DRIVER_USE_PART = 'DUP'
DRIVER_DECL_PART = 'DDP'
DRIVER_EXEC_PART ='DEP'
DRIVER_CONTAINS_PART = 'DCP'
DRIVER_SUBP_PART = 'DSP'
DRIVER_ALLOC_PART = 'DAP'
DRIVER_DEALLOC_PART = 'DDAP'
DRIVER_READ_IN_ARGS = 'DRIA'
DRIVER_CALLSITE_PART = 'DCP'

STATE_PBLOCK_USE_PART = 'SPBUP'
STATE_PBLOCK_DECL_PART = 'SPBDP'
STATE_PBLOCK_EXEC_PART ='SPBEP'
STATE_PBLOCK_CONTAINS_PART = 'SPBCP'
STATE_PBLOCK_SUBP_PART = 'SPBSP'
STATE_PBLOCK_WRITE_IN_ARGS = 'SPBWIA'
STATE_PBLOCK_WRITE_IN_EXTERNS = 'SPBWIE'
STATE_PBLOCK_WRITE_IN_LOCALS ='SPBWIL'
STATE_PBLOCK_WRITE_OUT_EXTERNS = 'SPBWOE'
STATE_PBLOCK_WRITE_OUT_LOCALS = 'SPBWOL'

STATE_TBLOCK_USE_PART = 'STBUP'
STATE_TBLOCK_DECL_PART = 'STBDP'
STATE_TBLOCK_CONTAINS_PART = 'STBCP'
STATE_TBLOCK_SUBP_PART = 'STBSP'

KERNEL_PBLOCK_USE_PART = 'KPBUP'
KERNEL_PBLOCK_DECL_PART = 'KPBDP'
KERNEL_PBLOCK_EXEC_PART ='KPBEP'
KERNEL_PBLOCK_CONTAINS_PART = 'KPBCP'
KERNEL_PBLOCK_SUBP_PART = 'KPBSP'
KERNEL_PBLOCK_READ_IN_EXTERNS = 'KPBRIE'
KERNEL_PBLOCK_READ_IN_LOCALS ='KPBRIL'
KERNEL_PBLOCK_READ_OUT_EXTERNS = 'KPBROE'
KERNEL_PBLOCK_READ_OUT_LOCALS = 'KPBROL'

KERNEL_TBLOCK_USE_PART = 'KTBUP'
KERNEL_TBLOCK_DECL_PART = 'KTBDP'
KERNEL_TBLOCK_CONTAINS_PART = 'KTBCP'
KERNEL_TBLOCK_SUBP_PART = 'KTBSP'

rprefix = 'kr'
wprefix = 'kw'
vprefix = 'kv'

MAXLEN_SUBPNAME = 55

def get_ancestor_name(stmt, generation):
    assert stmt and hasattr(stmt, 'parent'), 'Given stmt does not have parent attribute.'
    assert isinstance(generation, int), 'Not integer type of generation.'

    ancestor = stmt.ancestors()[generation]
    if hasattr(ancestor, 'name'):
        return ancestor.name 
    else: return ''

def get_topname(stmt):
    return get_ancestor_name(stmt, 0)

def get_parentname(stmt):
    return get_ancestor_name(stmt, -1)

def get_dtype_subpname(typestmt):
    assert typestmt, 'None type of typestmt'
    return '%s_%s'%(get_topname(typestmt), typestmt.name)

def get_typedecl_subpname(stmt, entity_name):
    import typedecl_statements
    if not hasattr(get_typedecl_subpname, 'kgen_subpname_cache'):
        get_typedecl_subpname.kgen_subpname_cache = {}

    assert isinstance(stmt, typedecl_statements.TypeDeclarationStatement), 'None type of typedecl stmt'
    assert entity_name, 'No entity name is provided.'

    var = stmt.get_variable(entity_name)
    if var is None: return 'Unknown_name'

    prefix = [ get_parentname(stmt), stmt.name ] + list(stmt.selector)
    l = []
    if var.is_array(): l.append('dim%d'%var.rank)
    if var.is_pointer(): l.append('ptr')

    subpname = '_'.join(prefix+l)
    if len(subpname)<MAXLEN_SUBPNAME:
        return '_'.join(prefix+l)
    else:
        if subpname in get_typedecl_subpname.kgen_subpname_cache:
            return 'kgen_subpname_%d'%get_typedecl_subpname.kgen_subpname_cache[subpname]
        else:
            subpindex = len(get_typedecl_subpname.kgen_subpname_cache)
            get_typedecl_subpname.kgen_subpname_cache[subpname] = subpindex
            return 'kgen_subpname_%d'%subpindex

def get_dtype_writename(typestmt):
    if typestmt is None: return
    subpname = get_dtype_subpname(typestmt)
    if subpname: return '%s_%s'%(wprefix, subpname)

def get_dtype_readname(typestmt):
    if typestmt is None: return
    subpname = get_dtype_subpname(typestmt)
    if subpname: return '%s_%s'%(rprefix, subpname)

def get_module_in_writename(modstmt):
    if modstmt is None: return
    return '%s_externs_in_%s'%(wprefix, modstmt.name)

def get_module_out_writename(modstmt):
    if modstmt is None: return
    return '%s_externs_out_%s'%(wprefix, modstmt.name)

def get_module_in_readname(modstmt):
    if modstmt is None: return
    return '%s_externs_in_%s'%(rprefix, modstmt.name)

def get_module_out_readname(modstmt):
    if modstmt is None: return
    return '%s_externs_out_%s'%(rprefix, modstmt.name)

def get_typedecl_writename(typestmt, entity_name):
    if typestmt is None: return
    subpname = get_typedecl_subpname(typestmt, entity_name)
    if subpname: return '%s_%s'%(wprefix, subpname)

def get_typedecl_readname(typestmt, entity_name):
    if typestmt is None: return
    subpname = get_typedecl_subpname(typestmt, entity_name)
    if subpname: return '%s_%s'%(rprefix, subpname)

def get_typedecl_verifyename(typestmt, entity_name):
    if typestmt is None: return
    subpname = get_typedecl_subpname(typestmt, entity_name)
    if subpname: return '%s_%s'%(vprefix, subpname)

def process_spec_stmts(stmt):
    if not stmt: return
    if not hasattr(stmt, 'spec_stmts'): return

    for spec_stmt in stmt.spec_stmts:
        node = spec_stmt.genkpair
        if not node: continue
        if not node.kgen_isvalid: continue
        if not hasattr(spec_stmt, 'geninfo') or len(spec_stmt.geninfo)==0: continue

        def is_uname(item, unames):
            import re
            iname = re.split('\(|\*|=', item)[0].strip()
            if iname in unames: return True
            else: return False


        if hasattr(spec_stmt, 'items'):
            new_items = []
            unames = list(set([ uname.firstpartname() for uname, req in KGGenType.get_state(spec_stmt.geninfo) ]))
            for item in spec_stmt.items:
                if is_uname(item, unames):
                    new_items.append(item)
            node.new_items = new_items
            node.kgen_use_tokgen = True
        else:
            pass
            # maybe specific handling per classes

def gen_write_istrue(pobj, var, ename):

    # if isarray

    if var.is_array() and not var.is_explicit_shape_array():
        attrs = {'expr': 'SIZE(%s)==1'%ename}
        ifsizeobj = part_append_gensnode(pobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

        attrs = {'expr': 'UBOUND(%s, 1)<LBOUND(%s, 1)'%(ename, ename)}
        ifarrobj = part_append_gensnode(ifsizeobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

        attrs = {'variable': 'kgen_istrue', 'sign': '=', 'expr': '.FALSE.'}
        part_append_gensnode(ifarrobj, EXEC_PART, statements.Assignment, attrs=attrs)

        attrs = {'expr': 'UBOUND(%s, 1)==0 .AND. LBOUND(%s, 1)==0'%(ename, ename)}
        part_append_gensnode(ifarrobj, EXEC_PART, block_statements.ElseIf, attrs=attrs)

        attrs = {'variable': 'kgen_istrue', 'sign': '=', 'expr': '.FALSE.'}
        part_append_gensnode(ifarrobj, EXEC_PART, statements.Assignment, attrs=attrs)

        part_append_gensnode(ifarrobj, EXEC_PART, block_statements.Else, attrs=attrs)

        attrs = {'variable': 'kgen_istrue', 'sign': '=', 'expr': '.TRUE.'}
        part_append_gensnode(ifarrobj, EXEC_PART, statements.Assignment, attrs=attrs)

        part_append_gensnode(ifsizeobj, EXEC_PART, block_statements.Else, attrs=attrs)

        attrs = {'variable': 'kgen_istrue', 'sign': '=', 'expr': '.TRUE.'}
        part_append_gensnode(ifsizeobj, EXEC_PART, statements.Assignment, attrs=attrs)
    else:
        attrs = {'variable': 'kgen_istrue', 'sign': '=', 'expr': '.TRUE.'}
        part_append_gensnode(pobj, EXEC_PART, statements.Assignment, attrs=attrs)

    # if allocatable
    if var.is_allocatable():
        attrs = {'expr': '.NOT. ALLOCATED(%s)'%ename}
        ifallocobj = part_append_gensnode(pobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

        attrs = {'variable': 'kgen_istrue', 'sign': '=', 'expr': '.FALSE.'}
        part_append_gensnode(ifallocobj, EXEC_PART, statements.Assignment, attrs=attrs)

    # if pointer
    if var.is_pointer():
        attrs = {'expr': '.NOT. ASSOCIATED(%s)'%ename}
        ifptrobj = part_append_gensnode(pobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

        attrs = {'variable': 'kgen_istrue', 'sign': '=', 'expr': '.FALSE.'}
        part_append_gensnode(ifptrobj, EXEC_PART, statements.Assignment, attrs=attrs)


    if (var.is_array() and not var.is_explicit_shape_array()) or var.is_allocatable() or var.is_pointer():

        attrs = {'items': ['kgen_istrue'], 'specs': ['UNIT = kgen_unit']}
        part_append_gensnode(pobj, EXEC_PART, statements.Write, attrs=attrs)

        attrs = {'expr': 'kgen_istrue'}
        iftrueobj = part_append_gensnode(pobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

        return iftrueobj
    else:
        return pobj


def gen_read_istrue(subrobj, var, ename, allocate=False):

    pobj = subrobj
 
    if (var.is_array() and not var.is_explicit_shape_array()) or var.is_allocatable() or var.is_pointer():
        attrs = {'items': ['kgen_istrue'], 'specs': ['UNIT = kgen_unit']}
        part_append_genknode(pobj, EXEC_PART, statements.Read, attrs=attrs)

        attrs = {'expr': 'kgen_istrue'}
        iftrueobj = part_append_genknode(pobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

        pobj = iftrueobj

    if var.is_allocatable() or allocate:
        attrs = {'expr': 'ALLOCATED( %s )'%ename}
        ifalloc = part_append_genknode(pobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

        attrs = {'items': ['%s'%ename]}
        part_append_genknode(ifalloc, EXEC_PART, statements.Deallocate, attrs=attrs)

    if var.is_pointer():
        attrs = {'expr': 'ASSOCIATED( %s )'%ename}
        ifalloc = part_append_genknode(pobj, EXEC_PART, block_statements.IfThen, attrs=attrs)

        attrs = {'items': ['%s'%ename]}
        part_append_genknode(ifalloc, EXEC_PART, statements.Nullify, attrs=attrs)

    return pobj


# code for Unit class which parses Config files 
# also exports code for UnitUnion class

__all__ = ["FlashUnit","UnitUnion"]

#########################################
import globals
from globals import *
from utils import *
from preProcess import preProcess

import re, os.path, types, string, copy, math

######## FLASH UNIT CODE ############

NO_RESTRICT = "noRestrict"
TOP_UNIT    = "topUnit"

## For every KEYWORD we have two methods
## initparseKEYWORD()
##   returns a pair (initialvalue,regexp)
##   initialvalue is the default value for internal DataStructure which handles this keyword
##   regexp is the regular expression for parsing this keyword line
## parseKEYWORD(self,mobj)
##   mobj is the match object instance returned by regexp parser
##   you need to update self["KEYWORD"] appropriately
class FlashUnit(dict,preProcess):
    """Encapsulates unit information as expressed in Config files.
    Data is accessed through dictionary methods, ex:

    FlashUnit('IO/common')['DEFAULT']
    FlashUnit('../Simulation/SimulationMain/Sedov')['LIBRARY']

    This is where the Config file syntax is defined.
    NOTE: we need to be in source/ when instantiating this.
    """

    firstcall = True        # is this the first time the class is instantiated
    regexps = {}            # mapping keywords to corresponding regexps
    keywords = []           # list of keywords
    initvalues = {}         # dictionary mapping keywords to default datastructure values

    COMMENT = '#'
    QUOTE   = '"'
    FILEBASE= 'Config'

    # prefixes for method names that go with different levels of restriction
    noRestrictPrefix = "parse"
    topUnitPrefix    = "topUnitParse"

    # parsers that go with different levels of restriction
    noRestrictParsers = []
    topUnitParsers = []

    # error messages that go with different levels of restriction
    noRestrictErrMsg = "Unknown keyword \"%s\" in file \"%s\", line %d."
    topUnitErrMsg    = ("Illegal keyword \"%s\" in file \"%s\", line %d.\n" +
                        "Only DEFAULT, CHILDORDER, PARAMETER, and 'D' keywords are allowed in a top-level Config.")

    def __init__(self, pathname, restrict, ignorePP=False):
        dict.__init__(self)
        preProcess.__init__(self,values=GVars.setupVars.getdict(), ignorePP=ignorePP)

        myClass = self.__class__
        if myClass.firstcall: self.init_class_vars()
        ivals = myClass.initvalues
        for key in list(ivals.keys()):
            self[key] = copy.copy(ivals[key])

        if not os.path.isdir(pathname): # given name does not make sense
            raise SetupError('Unit %s not found '% pathname)

        # else
        self.name = os.path.normpath(pathname) #something like 'IO/common/hdf5/'

        pathToConfig = os.path.join(self.name, self.FILEBASE)
        if os.path.isfile(pathToConfig):
           self.filename = pathToConfig
           self.prefix  = getattr(myClass, "%sPrefix" % restrict)  # e.g. 'noRestrictPrefix'
           self.parsers = getattr(myClass, "%sParsers" % restrict)
           self.errMsg  = getattr(myClass, "%sErrMsg" % restrict)
           self.parse()
        else:
           self.filename = ""

    def init_class_vars(self): # initialize class level variables
        FlashUnit.firstcall = False
        
        # This is just a clever way to list the methods in the
        # class. The dir(self.__class__) lists all the attributes
        # like __cmp__ , __init___, __setitem__, __len__ etc and
        # then also the user defined methods, in this case,
        # getParent, initParser, match, parseDEFAULT, parseEXCLUSIVE etc.
        myClass = self.__class__
        noRestrictRe = re.compile("%s[A-Z_]+$" % myClass.noRestrictPrefix)
        topUnitRe    = re.compile('%s[A-Z_]+$' % myClass.topUnitPrefix)
        for name in dir(myClass):
          if noRestrictRe.match(name):
            if type(getattr(self, name)) != types.MethodType: continue
            # we got a keyword
            keyword = name[5:] # remove the parse
            myClass.noRestrictParsers.append(name)
            myClass.keywords.append(keyword) 
            # get the initial value and regexp
            initval,regexp = getattr(myClass, "initparse%s"%keyword)()
            myClass.initvalues[keyword] = initval
            myClass.regexps[keyword] = re.compile(regexp)
          elif topUnitRe.match(name):
            if type(getattr(self, name)) != types.MethodType: continue
            myClass.topUnitParsers.append(name)

        #HACK for STRING PARAMETERS since "parse" doesn't handle string parameters
        myClass.regexps['pstring']=re.compile(r'PARAMETER\s+(\w+)\s+'\
                                              r'STRING\s+(?:CONSTANT\s+)'
                                              r'?(".*?")\s*(?:[\[](.*)[]])?\s*(?:#.*)?$')

    def __cmp__(self, other):
        """Alphabetical comparison on unit names (like 'source/io/amr')"""
        if isinstance(other,str):
            return cmp(self.name, other)
        else:
            return cmp(self.name, other.name)

    def __repr__(self):
        return "FlashUnit instance for %s" % self.name
        
    def getParent(self):
        """Returns parent unit's full name, empty string if we're orphans"""
        return os.path.split(self.name)[0] #FIXME ../simulations/ case
        
    def match(self, keyword, line):
        regexps = self.__class__.regexps
        match = regexps[keyword].match(line)
        if not match:
            raise SetupError('input doesn\'t match regular expression "%s"'%\
                             regexps[keyword].pattern)
        return match

    def parse(self):
        ans = self.processFile(self.filename) # exception goes all the way to the top

        prefix  = self.prefix
        parsers = self.parsers
        errMsg  = self.errMsg
        for lineno,line in ans:
            rawline = line
            if rawline and rawline[-1]=='\n': rawline = rawline[:-1]

            line=stripComments(line, self.COMMENT,self.QUOTE)
            line=str.strip(line)
            if not line: continue

            keyword = str.split(line)[0]
            pkeyword = prefix + keyword  # e.g. 'topUnitParse' + 'DEFAULT'
            if pkeyword not in parsers:
                raise SetupError(errMsg % (keyword, self.filename, lineno))
            try:
                self.rawline = rawline #FIXME this is for STRING PARAM hack
                mobj = self.match(keyword,line)
                getattr(self,pkeyword)(mobj)
            except SetupError as msg:
                # Do not abort on multiple occurrence of same PARAMETER in the same file
                # if we are only parsing for documentation purposes (ignorePP is true) - KW
                if not self.ignorePP or not (str(msg)[-17:]==" already declared"):
                    raise SetupError('Bad syntax: file %s, line %d:\n%s\n\n%s' % \
                                     (self.filename, lineno, rawline, str(msg)))

    @staticmethod
    def initparseCHILDORDER():
        return [], r'CHILDORDER\s+(.*)$'

    def parseCHILDORDER(self, mobj):
        # CHILDORDER = order in which some of the children must be processed
        # remaining children appear later in lexicographic order
        self['CHILDORDER'] = mobj.group(1).split()

    @staticmethod
    def initparseDATAFILES():
        return [], r'DATAFILES\s+(.*)$'

    def parseDATAFILES(self, mobj):
        # DATAFILES -> list of wildcard patterns (with absolute path)
        self['DATAFILES'].extend([os.path.join(self.name,x) for x in mobj.group(1).split()])

    @staticmethod
    def initparseVARIANTS():
        return [], r'VARIANTS\s+(.*)$'

    def parseVARIANTS(self, mobj):
        # VARIANTS -> list of wildcard patterns
        self['VARIANTS'].extend([x for x in mobj.group(1).split()])

    @staticmethod
    def initparseD():
        return {}, r'D\s*(&|\S+)\s*(.*)$'

    def parseD(self, mobj):
        key, comment = mobj.groups()
        if key=='&':
           if self.DKey in self['D']:
              self['D'][self.DKey] = "%s %s" % (self['D'][self.DKey],comment)
           else: raise SetupError('Improper usage of comment continuation')
        else:
            self.DKey = key
            self['D'][key] = comment
            
    @staticmethod
    def initparseFACEVAR():
        strEosRE  = r'(\s+EOSMAP:\s*(?P<eosmap>' + GVars.strEos + r')\s*$)?'
        return {},r'FACEVAR\s+(?P<varname>\w+)' + strEosRE

    def parseFACEVAR(self,mobj):
        facevar = mobj.group("varname")
        eosmap = mobj.group("eosmap")
        if not eosmap: eosmap = "NONEXISTENT"
        if facevar in self['FACEVAR']:
           raise SetupError('FACEVAR %s already declared'%facevar)
        self['FACEVAR'][facevar] = (eosmap.upper(),eosmap.upper())

    @staticmethod
    def initparseVARIABLE():
        #Order is important.  Only Type then Eos is valid.
#        strTypeRE = '(\s+TYPE:\s*(?P<type>GENERIC|PER_VOLUME|PER_MASS))?'
        strTypeRE = r'TYPE:\s*(?P<type>GENERIC|PER_VOLUME|PER_MASS)'
        strEosmapRE  = r'EOSMAP(INOUT)?:\s*(?P<eosmap>' + GVars.strEos + r')|EOSMAPIN:\s*(?P<eosmapin>' + GVars.strEos + r')|EOSMAPOUT:\s*(?P<eosmapout>' + GVars.strEos + r')'
        strEosRE  = r'(\s+EOSMAP:\s*(?P<eosmap>' + GVars.strEos + r')\s*$)?'
#        return {},'VARIABLE\s+(?P<varname>\w+)' + strTypeRE + strEosRE
        return {},r'VARIABLE\s+(?P<varname>\w+)(\s+)?(?:(' + strTypeRE + r'|' + strEosmapRE + r')\s*)*\s*$'
#        return {}, r'MASS_SCALAR\s+(?P<msname>\w+)(\s+)?(?:(NORENORM|RENORM:(?P<gpname>\S*)|' + strEosmapRE

    def parseVARIABLE(self, mobj):
        variable = mobj.group("varname").lower()
        vartype = mobj.group("type")
        eosmap = mobj.group("eosmap")
        eosmapin = mobj.group("eosmapin")
        eosmapout = mobj.group("eosmapout")
        if eosmapin and eosmapout and eosmap and (eosmap != eosmapin) and (eosmapout != eosmap):
            raise SetupError('VARIABLE %s has EOSMAP as well as EOSMAPIN and EOSMAPOUT mappings, what gives?'%variable)
        if not eosmapin: eosmapin = eosmap
        if not eosmapout: eosmapout = eosmap
        if not eosmapin: eosmapin = "NONEXISTENT"
        if not eosmapout: eosmapout = "NONEXISTENT"

        if not vartype: vartype = "GENERIC"
        if variable in self['VARIABLE']:
            raise SetupError('VARIABLE %s already declared'%variable)
        self['VARIABLE'][variable] = (vartype.upper(), eosmapin.upper(), eosmapout.upper())

    @staticmethod
    def initparseEVOLVEDVAR():
        return [], r'EVOLVEDVAR\s+(.*)$'
    
    def parseEVOLVEDVAR(self, mobj):
        # EVOLVEDVAR -> evolved variable(s)
        for var in mobj.group(1).lower().split():
            self['EVOLVEDVAR'].append(var)

    @staticmethod
    def initparseNRHS():
        return None, r"NRHS\s+([0-9]+)$"
    
    def parseNRHS(self, mobj):
        # Make sure that only a MoL unit set this
        if not self.name.startswith("TimeAdvance/TimeAdvanceMain/MoL"):
            raise SetupError("NRHS can only be declared in a MoL implementation")
        
        nrhs = int(mobj.group(1))
        if self['NRHS'] != None:
            raise SetupError('NRHS already declared')
        self['NRHS'] = nrhs

    @staticmethod
    def initparseLIBRARY():
        return {}, r'LIBRARY\s+(\S+)\s*(.*)$'

    def parseLIBRARY(self, mobj):
        libname = mobj.group(1).lower()
        libargs = " ".join(mobj.group(2).split()) # trims and removes multiple spaces
        self['LIBRARY'][libname] = libargs

    @staticmethod
    def initparseGUARDCELLS():
        return None, r'GUARDCELLS\s+([0-9]+)$'

    def parseGUARDCELLS(self, mobj):
        num = int(mobj.group(1))
        if self['GUARDCELLS']!=None:
            raise SetupError('GUARDCELLS already declared')
        self['GUARDCELLS'] = num

    @staticmethod
    def initparseFLUX():
        return {}, r'FLUX\s+(\w+)$'

    def parseFLUX(self, mobj):
        self['FLUX'][mobj.group(1)] = 1
                
    @staticmethod
    def initparseSCRATCHVAR():
        strEosRE  = r'(\s+EOSMAP:\s*(?P<eosmap>' + GVars.strEos + r')\s*$)?'
        return {},r'SCRATCHVAR\s+(?P<varname>\w+)' +  strEosRE

    def parseSCRATCHVAR(self, mobj):
        variable = mobj.group("varname")
        eosmap = mobj.group("eosmap")
        if not eosmap: eosmap = "NONEXISTENT"
        self['SCRATCHVAR'][variable] = (eosmap.upper(),eosmap.upper())

    @staticmethod
    def initparseSCRATCHCENTERVAR():
        strEosRE  = r'(\s+EOSMAP:\s*(?P<eosmap>' + GVars.strEos + r')\s*$)?'
        return {},r'SCRATCHCENTERVAR\s+(?P<varname>\w+)' +  strEosRE

    def parseSCRATCHCENTERVAR(self, mobj):
        variable = mobj.group("varname")
        eosmap = mobj.group("eosmap")
        if not eosmap: eosmap = "NONEXISTENT"
        self['SCRATCHCENTERVAR'][variable] = (eosmap.upper(),eosmap.upper())

    @staticmethod
    def initparseNSCRATCHCENTERVARS():
        return None, r'NSCRATCHCENTERVARS\s+(?P<varnum>\w+)\s*$'

    def parseNSCRATCHCENTERVARS(self, mobj):
        varnumStr = mobj.group("varnum")
        if self['NSCRATCHCENTERVARS']!=None:
            raise SetupError('NSCRATCHCENTERVARS already declared')
        self['NSCRATCHCENTERVARS'] = varnumStr

    @staticmethod
    def initparseSCRATCHFACEXVAR():
        strEosRE  = r'(\s+EOSMAP:\s*(?P<eosmap>' + GVars.strEos + r')\s*$)?'
        return {},r'SCRATCHFACEXVAR\s+(?P<varname>\w+)' +  strEosRE

    def parseSCRATCHFACEXVAR(self, mobj):
        variable = mobj.group("varname")
        eosmap = mobj.group("eosmap")
        if not eosmap: eosmap = "NONEXISTENT"
        self['SCRATCHFACEXVAR'][variable] = (eosmap.upper(),eosmap.upper())

    @staticmethod
    def initparseSCRATCHFACEYVAR():
        strEosRE  = r'(\s+EOSMAP:\s*(?P<eosmap>' + GVars.strEos + r')\s*$)?'
        return {},r'SCRATCHFACEYVAR\s+(?P<varname>\w+)' +  strEosRE

    def parseSCRATCHFACEYVAR(self, mobj):
        variable = mobj.group("varname")
        eosmap = mobj.group("eosmap")
        if not eosmap: eosmap = "NONEXISTENT"
        self['SCRATCHFACEYVAR'][variable] = (eosmap.upper(),eosmap.upper())

    @staticmethod
    def initparseSCRATCHFACEZVAR():
        strEosRE  = r'(\s+EOSMAP:\s*(?P<eosmap>' + GVars.strEos + r')\s*$)?'
        return {},r'SCRATCHFACEZVAR\s+(?P<varname>\w+)' +  strEosRE

    def parseSCRATCHFACEZVAR(self, mobj):
        variable = mobj.group("varname")
        eosmap = mobj.group("eosmap")
        if not eosmap: eosmap = "NONEXISTENT"
        self['SCRATCHFACEZVAR'][variable] = (eosmap.upper(),eosmap.upper())

    @staticmethod
    def initparseSPECIES():
        return {}, r'SPECIES\s+(?P<name>\w+)(\s+TO\s+(?P<nElec>\d+))?\s*$'

    def parseSPECIES(self, mobj):
        name = mobj.group("name")
        nStrElec = mobj.group("nElec")

        #If the number of electrons are specified in the Config file then
        #nStrElec is not none.  We store the integer representation of this
        #value in the dictionary, or a 1 if this value is not specified.
        if nStrElec:
            nElec = int(nStrElec)
            if nElec < 1:
                raise SetupError('Must be at least one electron in each species.')
            self['SPECIES'][name] = nElec
        else:
            self['SPECIES'][name] = 1

    @staticmethod
    def initparseMASS_SCALAR():
#        strEosRE  = '(\s+EOSMAP:\s*(?P<eosmap>' + GVars.strEos + ')\s*$)?'
        strEosmapRE  = r'EOSMAP(INOUT)?:\s*(?P<eosmap>' + GVars.strEos + r')|EOSMAPIN:\s*(?P<eosmapin>' + GVars.strEos + r')|EOSMAPOUT:\s*(?P<eosmapout>' + GVars.strEos + r')'
        return {}, r'MASS_SCALAR\s+(?P<msname>\w+)(\s+)?(?:(NORENORM|RENORM:(?P<gpname>\S*)|' + strEosmapRE + r')\s*)*\s*$'

    def parseMASS_SCALAR(self, mobj):
        msname = mobj.group("msname")
        gpname = mobj.group("gpname")
        if gpname: gpname = str(gpname).upper()
        eosmap = mobj.group("eosmap")
        eosmapin = mobj.group("eosmapin")
        eosmapout = mobj.group("eosmapout")
        if eosmapin and eosmapout and eosmap and (eosmap != eosmapin) and (eosmapout != eosmap):
            raise SetupError('MASS_SCALAR %s has EOSMAP as well as EOSMAPIN and EOSMAPOUT mappings, what gives?'%msname)
        if not eosmapin: eosmapin = eosmap
        if not eosmapout: eosmapout = eosmap
        if not eosmapin: eosmapin = "NONEXISTENT"
        if not eosmapout: eosmapout = "NONEXISTENT"
        self['MASS_SCALAR'][msname] = (gpname, eosmapin.upper(), eosmapout.upper())
##        print "self['MASS_SCALAR'][" + msname + "] <-", self['MASS_SCALAR'][msname] 


    @staticmethod
    def initparsePARTICLEPROP():
        return {}, r'PARTICLEPROP\s+(?P<name>\w+)\s+(?P<type>INTEGER|REAL)\s*$'

    def parsePARTICLEPROP(self, mobj):
        name = mobj.group("name")
        prop_type = mobj.group("type")
        if prop_type=="INTEGER":
            raise SetupError('Particle properties of type INTEGER are currently not supported, use REAL instead to store integer %s information!'%name)
        if name in self['PARTICLEPROP']:
            raise SetupError('PARTICLEPROP %s already declared'%name)
        self['PARTICLEPROP'][name] = prop_type

    #We will store all of the particle type information as a dictionary of tuples.
    #The particle type will be key, and the values will be the map & init methods.
    @staticmethod
    def initparsePARTICLETYPE():
        return {}, r'PARTICLETYPE\s+(?P<particleType>\w+)\s+INITMETHOD\s+(?P<initMethod>\w+)\s+MAPMETHOD\s+(?P<mapMethod>\w+)\s+ADVMETHOD\s+(?P<advMethod>\w+)\s*$'

    def parsePARTICLETYPE(self, mobj):
        particleType = mobj.group("particleType")
        initMethod = mobj.group("initMethod")
        mapMethod = mobj.group("mapMethod")
        advMethod = mobj.group("advMethod")
        if particleType in self['PARTICLETYPE']:
            raise SetupError('PARTICLETYPE %s already declared' % particleType)
        self['PARTICLETYPE'][particleType] = (initMethod, mapMethod, advMethod)

    @staticmethod
    def initparsePARTICLEMAP():
        return {}, r'PARTICLEMAP\s+(TO)\s+(?P<name>\w+)\s+(FROM)\s+(?P<type>'\
            r'(SCRATCHVAR|SCRATCHCENTERVAR|SCRATCHFACEXVAR|SCRATCHFACEYVAR|SCRATCHFACEZVAR|FACEX'\
            r'|FACEY|FACEZ|VARIABLE|MASS_SCALAR|SPECIES))\s+(?P<varname>\w+)\s*$'

    def parsePARTICLEMAP(self,mobj):
        name = mobj.group("name")
        type = mobj.group("type")
        varname = mobj.group("varname")
        if name in self['PARTICLEMAP']:
            raise SetupError('PARTICLEMAP %s already set'%name)
        self['PARTICLEMAP'][name] = (type, varname)
            
    @staticmethod
    def initparseSUGGEST():
        return [],r'SUGGEST\s+(.*)$'

    def parseSUGGEST(self,mobj):
        sugset = []
        for u in mobj.groups()[0].split():
            if u: 
               if u[0]==".": # using relative path
                  sugset.append(os.path.normpath(os.path.join(self.name,u)))
               else:
                  sugset.append(os.path.normpath(u))
        self['SUGGEST'].append(sugset)

    @staticmethod
    def initparseREQUIRES():
        # "REQUIRES" can be followed by a list of potential Units separated by "OR"
        return [], r'REQUIRES\s+(?P<orList>(?:\S+|\s+OR\s+)+)\s*$'

    def parseREQUIRES(self, mobj):
        def normpath(path):
            if path.startswith("."):
                path = os.path.join(self.name, path)
            return os.path.normpath(path)

        # Make a list of all units that followed the "REQUIRES" as delimited by "OR"
        # If only one unit was given, (i.e. there were no "OR"s), re.split() will
        # return a list of one.
        units = re.split(r"\s+OR\s+", mobj.group("orList"))
        # change any relative paths to absolute paths; remove any empties
        units = [normpath(unit) for unit in units if len(unit) > 0]
        # Note that self['REQUIRES'] is a list of lists
        self['REQUIRES'].append(units)

    @staticmethod
    def initparseREQUESTS():
        return [], r'REQUESTS\s+(?P<name>\S+)\s*$'

    # same as REQUIRES except can be negated by a -without-unit in cmd line
    # also only one unit at a time
    def parseREQUESTS(self, mobj):
        unit = ""
        u = mobj.group("name")
        if u: 
           if u[0]==".": # using relative path
              unit = os.path.normpath(os.path.join(self.name,u))
           else:
              unit = os.path.normpath(u)
        # do we need to REQUIRE it or not?
        ignore = False # do not ignore this unit
        reason = "Unknown"
        for x in list(GVars.withoutUnits.keys()):
            if unit == x: 
               ignore = True
               reason = x
            elif unit.startswith(x+os.sep):
               ignore = True
               reason = x
        if not ignore: # pretend this was a REQUIRES
           self['REQUIRES'].append([unit])
           GVars.out.put("Honoring request for %s"%unit,globals.DEBUG)
        else:
           GVars.out.put("Ignoring request for %s (reason: -without-unit=%s)"%(unit,reason),globals.DEBUG)

    @staticmethod
    def initparsePARAMETER():
        return {}, r'PARAMETER\s+(\w+)\s+'\
                   r'(INTEGER|REAL|STRING|BOOLEAN)\s+(?:(CONSTANT)\s+)?'\
                   r'([^[]+)(?:[\[](.*)[]])?\s*$'

    def parsePARAMETER(self, mobj):
        name,type,constant,value,range = mobj.groups()
        if name in self['PARAMETER']:
            raise SetupError('%s already declared'%name)
        if type=='STRING':
            #This is a hack. I "forgot" to accomodate parsers that would need
            #rawlines when I wrote the general framework. See init_class_vars.
            name, value,range = self.match('pstring', self.rawline).groups()
        else:
            value = str.rstrip(value)
        self['PARAMETER'][name]=(type, value, constant, range)

    @staticmethod
    def initparseDEFAULT():
        return "", r'DEFAULT\s+(\S+)$'

    def parseDEFAULT(self, mobj):
        self['DEFAULT'] = os.path.join(self.name, os.path.normpath(mobj.group(1)))
        
    @staticmethod
    def initparseEXCLUSIVE():
        return [], r'EXCLUSIVE\s+(.+)$'

    def parseEXCLUSIVE(self, mobj):
        units = str.split(mobj.group(1))

        #look explicitly for the "*" which means only one directory
        #below can be chosen, ie all are exclusive

        if units[0] == "*":
            dirs = os.listdir(self.name)
            for dir in dirs:
                if (dir[0] != '.') and (os.path.isdir(os.path.join(self.name, dir))): 
                   units.append(os.path.join(self.name, dir))
            units.remove("*")       
        else:
            units = [os.path.join(self.name,os.path.normpath(x)) for x in units]

        self['EXCLUSIVE'].append(units)

    @staticmethod
    def initparseKERNEL():
        return None, r'KERNEL(\s+([^ \t]+))?\s*$'

    def parseKERNEL(self, mobj):
        GVars.out.put("parseKERNEL: found %s in %s"%(mobj.group(2),self),globals.DEBUG)
        if not mobj.group(2):
            self['KERNEL'] = self.name
        else:
            kname = os.path.join( self.name, mobj.group(2))
            self['KERNEL'] = kname
        GVars.out.put("parseKERNEL: set to %s."% self['KERNEL'],globals.DEBUG)

    @staticmethod
    def initparseLINKIF():
        return [], r'LINKIF\s+(.+)$'

    def parseLINKIF(self,mobj):
        # LINKIF spaces non-spaces spaces non-spaces spaces ENDOFLINE
        # first set of non-spaces is called "filename" and the second set "unitname"
        parts = str.split(mobj.group(1))
        # We are appending a pair (filename,unitname)
        if not self.name.startswith(globals.SimSetupDirname):
          GVars.out.put("WARNING: \"LINKIF\" directive found in file \"%s\"" % os.path.join(self.name, self.FILEBASE))
          GVars.out.push()
          GVars.out.put("\"LINKIF\" directives should only be used with the simulations")
          GVars.out.put("directory \"%s\"" % globals.SimSetupDirname)
          GVars.out.pop()

        self['LINKIF'].append((os.path.join(self.name,parts[0]),parts[1]))

    @staticmethod
    def initparseCONFLICTS():
        return {}, r'CONFLICTS\s+(.+)$'

    def parseCONFLICTS(self, mobj):
        # CONFLICTS space MODNAME space MODNAME ....
        # raise an error if any of MODNAME are also included
        for unit in mobj.group(1).split(): self['CONFLICTS'][unit] = 1

    @staticmethod
    def initparsePPDEFINE():
        # name one pre-processor symbol and optionally value to define
        return {},r'PPDEFINE\s+(?P<sym>\w+)(?:\s+(?P<val>\w+))?$'

    def parsePPDEFINE(self,mobj):
        self['PPDEFINE'][mobj.group("sym")] = mobj.group("val")
    
    @staticmethod
    def initparseNONREP():
        # non mesh replicated variable array 
        return {},r'NONREP\s+(?P<type>MASS_SCALAR|VARIABLE)\s+(?P<name>\w+)\s+(?P<nloc>[0-9]+)\s+(?P<pglob>\w+)(\s+(?P<namef>\S+))?$'
    def parseNONREP(self,mobj):
        tp = mobj.group("type")
        name = mobj.group("name")
        nlocs = int(mobj.group("nloc"))
        pglob = mobj.group("pglob")
        namef = (mobj.group("namef") or "%(name)s???") % { 'name': name }
        tpre = self.__class__.regexps[tp]
        tpparse = getattr(self, 'parse%s' % tp)
        locf = 'NONREP_%s_%X_%%06d' % (name, name.__hash__() & 0xffffffff)
        for i in range(nlocs):
            tpparse(tpre.match('%s %s' % (tp, locf % (i+1))))
        self['NONREP'][name] = { 'tp':tp, 'rpcount':pglob, 'nlocs':nlocs, 'locf':locf, 'namef':namef }
    # These methods are called when 'restrict' is TOP_UNIT
    # They are just alternate handles to regular parse methods.
    topUnitParseD          = parseD
    topUnitParseDEFAULT    = parseDEFAULT
    topUnitParsePARAMETER  = parsePARAMETER
    topUnitParseCHILDORDER = parseCHILDORDER
######################################################

class UnitUnion(dict):
    """Collates info from an ordered list of FlashUnit instances"""

    def __init__(self, units):
        """
        'units' is a list of FlashUnit instances
        """
        dict.__init__(self)
        self['DATAFILES'] = []
        self['VARIANTS'] = []
        self['FACEVAR'] = {}
        self['FLUX'] = {}
        self['SCRATCHVAR'] = {}
        self['SCRATCHCENTERVAR'] = {}
        self['NSCRATCHCENTERVARS'] = None
        self['SCRATCHFACEXVAR'] = {}
        self['SCRATCHFACEYVAR'] = {}
        self['SCRATCHFACEZVAR'] = {}
        self['GUARDCELLS'] = None
        self['LIBRARY'] = {}
        self['PARTICLEPROP'] = {}
        self['PARTICLETYPE'] = {}
        self['PARTICLEMAP'] = {}
        self['SPECIES'] = {}
        self['MASS_SCALARS'] = {}
        self['MASS_SCALAR_GROUPS'] = {} # list of names of groups (in arbitrary order)
        self['LINKIF'] = []
        self['VARIABLE'] = {}
        self['EVOLVEDVAR'] = []
        self['NRHS'] = None
        self['PPDEFINES'] = {}
        self['NONREP'] = {}
        self.unitNames = [] # list of names of all the units
        # this will be filled by unitUtils
        
        self.collate(units)
        self.setDefaults()
        self.simplify()

    def collate(self, units):
        #varlocs = {} # dictionary mapping variables to location (for err msg only) ... NOT USED ANYWHERE.
        partproplocs = {} # for err msg
        speclocs = {} # dictionary storing which units define which species.
        mscalarlocs = {} # for err msg
        for unit in units:

            # self['LIBRARY'] is a dictionary of lists of arguments
            # In perfect world all entries of this list will be the same
            for lib in list(unit['LIBRARY'].keys()):
                if lib not in self['LIBRARY']: self['LIBRARY'][lib] = []
                self['LIBRARY'][lib].append(unit['LIBRARY'][lib])

            updateAndMergeVariablePropertyTuples(self['SCRATCHVAR'],unit['SCRATCHVAR'], "SCRATCHVAR")
            updateAndMergeVariablePropertyTuples(self['SCRATCHCENTERVAR'],unit['SCRATCHCENTERVAR'], "SCRATCHCENTERVAR")
            updateAndMergeVariablePropertyTuples(self['SCRATCHFACEXVAR'],unit['SCRATCHFACEXVAR'], "SCRATCHFACEXVAR")
            updateAndMergeVariablePropertyTuples(self['SCRATCHFACEYVAR'],unit['SCRATCHFACEYVAR'], "SCRATCHFACEYVAR")
            updateAndMergeVariablePropertyTuples(self['SCRATCHFACEZVAR'],unit['SCRATCHFACEZVAR'], "SCRATCHFACEZVAR")
            updateAndMergeVariablePropertyTuples(self['FACEVAR'],unit['FACEVAR'], "FACEVAR")
            self['FLUX'].update(unit['FLUX'])
            updateAndMergeVariablePropertyTuples(self['VARIABLE'],unit['VARIABLE'], "VARIABLE")
            self['PPDEFINES'].update(unit['PPDEFINE'])
            self['DATAFILES'].extend(unit['DATAFILES'])

            for evar in unit['EVOLVEDVAR']:
                if evar not in self['EVOLVEDVAR']: # strip out duplicates
                    self['EVOLVEDVAR'].append(evar)

            if unit["NRHS"] != None:
                if self["NRHS"] == None:
                    self["NRHS"] = unit["NRHS"]
                else:
                    self["NRHS"] += unit["NRHS"]

 
            self["NONREP"].update(unit["NONREP"])
            
            for name,(group,eos1,eos2) in list(unit["MASS_SCALAR"].items()):
                try:
                  prevTriple = self["MASS_SCALARS"][name]
                  prevgrp    = prevTriple[0]
                  gotval = True
                except KeyError:
                  self["MASS_SCALARS"][name] = (group, eos1, eos2)
                  gotval = False

                if gotval and (prevgrp != group): # different groups specified
                   msg = ['MASS SCALAR [%s] has two groups: ' % name]
                   msg.append('    %s in %s' % (prevgrp,unit.name))
                   msg.append('    %s in %s' % (group,mscalarlocs[name]))
                   raise SetupError("\n".join(msg))
                if gotval and (prevTriple[1] != eos1): # different EOS IN mappings specified
                   msg = ['MASS SCALAR [%s] has two mappings to EOS input:' % name]
                   msg.append('    %s in %s' % (prevTriple[1],unit.name))
                   msg.append('    %s in %s' % (eos1,mscalarlocs[name]))
                   raise SetupError("\n".join(msg))
                if gotval and (prevTriple[2] != eos2): # different EOS OUT mappings specified
                   msg = ['MASS SCALAR [%s] has two mappings from EOS output:' % name]
                   msg.append('    %s in %s' % (prevTriple[2],unit.name))
                   msg.append('    %s in %s' % (eos2,mscalarlocs[name]))
                   raise SetupError("\n".join(msg))
                # everything is fine now
                mscalarlocs[name] = unit.name # store where it was defined
                if group: self['MASS_SCALAR_GROUPS'][group] = 1


            #Check whether we have already encountered a species of the same
            #name.  If so, let the user know which Config files contain
            #the same SPECIES definition.  Otherwise, add the species to
            #the main dictionary.
            for (keyString, numElec) in list(unit['SPECIES'].items()):
                if keyString in self['SPECIES']:
                    #We have defined the same species in multiple Config files.
                    msg = ['SPECIES %s defined twice' % keyString]
                    msg.append('    %s defines the species %s' % (unit.name,unit['SPECIES']))
                    msg.append('    %s defines the species %s' % (speclocs[keyString],self['SPECIES']))
                    raise SetupError("\n".join(msg))                    
                else:                    
                    self['SPECIES'][keyString] = numElec    #Copy species into main dictionary.
                    speclocs[keyString] = unit.name         #Store unit name that defines species.


            for ud in unit['LINKIF']:
                if ud not in self['LINKIF']:
                    self['LINKIF'].append(ud)

            if unit['GUARDCELLS']!=None:
               if self['GUARDCELLS']==None:
                  self['GUARDCELLS']=unit['GUARDCELLS']
               else:
                  self['GUARDCELLS']= max(unit['GUARDCELLS'], self['GUARDCELLS'])

            if unit['NSCRATCHCENTERVARS']!=None:
               if self['NSCRATCHCENTERVARS']==None:
                  self['NSCRATCHCENTERVARS']=unit['NSCRATCHCENTERVARS']
               elif unit['NSCRATCHCENTERVARS'].isdecimal() and self['NSCRATCHCENTERVARS'].isdecimal():
                  self['NSCRATCHCENTERVARS']= "%s" % max(int(unit['NSCRATCHCENTERVARS']), int(self['NSCRATCHCENTERVARS']))
               else:
                  self['NSCRATCHCENTERVARS']= "max(%s,%s)" % (unit['NSCRATCHCENTERVARS'], self['NSCRATCHCENTERVARS'])

            for (prop,prop_type) in list(unit['PARTICLEPROP'].items()):
                if unit['PARTICLEPROP'].get(prop,prop_type)!=prop_type:
                   msg = ['Integer/Real PARTICLEPROP MISMATCH with %s ' % prop]
                   msg.append('    has type %s in %s' % (prop_type,unit.name))
                   msg.append('    has type %s in %s' % (self['PARTICLEPROP'][prop],partproplocs[prop]))
                   raise SetupError("\n".join(msg))
                else:
                   self['PARTICLEPROP'][prop]= prop_type
                   partproplocs[prop] = unit.name

            if unit['PARTICLETYPE'] != None:
                unitParticleTypeKeys = list(unit['PARTICLETYPE'].keys())
                for particleType in unitParticleTypeKeys:       
                    if particleType in self['PARTICLETYPE']:
                        raise SetupError('PARTICLETYPE %s already declared' % particleType)
                #We've ensured we have no common keys, so copy the data across:
                self['PARTICLETYPE'].update(unit['PARTICLETYPE'])

            for prop, map in list(unit['PARTICLEMAP'].items()):
                if unit['PARTICLEMAP'].get(prop,map)!=map:
                   msg = ['PARTICLEMAP  mismatch with %s ' % prop]
                   raise SetupError("\n".join(msg))
                else:
                   self['PARTICLEMAP'][prop] = map
                   
    def setDefaults(self):
  
        if self['GUARDCELLS']==None:
            self['GUARDCELLS'] = 4
            GVars.out.put('number of guard cells not specified, using default'\
                          ' GUARDCELLS 4',globals.INFO)
            
    def simplify(self):
        """Store stuff that can be computed from Config info in the dictionary
        We'll use lowercase keys to separate things.

        In many cases self['UPPERCASE'] is a dictionary. We define 
        self['uppercase'] as its sorted list of keys. """

        ppd = self['PPDEFINES'] # Speeds up lookup time
        ppds = []
        for x in list(ppd.keys()):
            if ppd[x]:
               ppds.append("#define %s %s" % (x.upper(),ppd[x]))
            else: ppds.append("#define %s" % x.upper())
        ppds.sort()
        self['ppdefines'] = ppds

        #need this since order in which a dict returns its keys is not determined.
        self['variable']= list(self['VARIABLE'].keys())
        self['variable'].sort()

        # Keep the relative order of EVOLVEDVAR variables;
        # they used to get sorted alphabetically here for MoL.
        self["evolvedvar"] = self["EVOLVEDVAR"]

        for evo in self["evolvedvar"]:
            if evo not in self['variable']:
                raise SetupError("ERROR! '%s' occurs in EVOLVEDVAR but is not declared as a VARIABLE" %
                                 evo)

        # Count of evolved variables for MoL.
        self["nevol"] = 0
        if GVars.setupVars.get("TimeAdvance").upper() == "MOL":
            self["nevol"] = len(self["evolvedvar"])
        # Move evolved variables to the front of UNK, whether MoL is included or not.
        self["variable"] = self["evolvedvar"] + [v for v in self["variable"] if v not in self["evolvedvar"]]

        tmpList = [ self['VARIABLE'][var] for var in self['variable'] ]
        self['var_types'] = [x for (x,y,z) in tmpList] # list of corresponding TYPES
        self['eosmapin_unkvars'] = [y for (x,y,z) in tmpList] # list of Eos_map roles
        self['eosmapout_unkvars'] = [z for (x,y,z) in tmpList] # list of Eos_map roles

        self['nparticletypes'] = len(self['PARTICLETYPE'])
        #Add a quick sense check.  Check that a simulation having 0 particle
        #types really does not use particles.
        particleTypesIncluded = (self['nparticletypes'] > 0)
        if (particleTypesIncluded != GVars.particlesUnitIncluded):
            raise SetupError('ERROR! Particle conflict\nHave we included a particle unit: %s.\n'
                             'Have we included a particle type: %s.' %
                             (GVars.particlesUnitIncluded, particleTypesIncluded))

        #When we define multiple particle types we must add a field named 
        #TYPE_PART_PROP so FLASH can distinguish between particle types.
        if 'type' in self['PARTICLEPROP']:
            raise SetupError('ERROR! TYPE_PART_PROP is a reserved definition\n')
        else:
            if self['nparticletypes'] > 1:
                self['PARTICLEPROP']['type'] = 'REAL'


        self['intproperty']  = []
        self['realproperty'] = []
        pkeys = list(self['PARTICLEPROP'].keys())
        #DEV: not using intproperty right now
        self['intproperty'] = [x for x in pkeys if self['PARTICLEPROP'][x] == 'INTEGER']
        self['intproperty'].sort()
        if self['intproperty']:
          self['n_int_props'] = len(self['intproperty'])
        else:
          self['n_int_props'] = 1

        self['realproperty'] = [x for x in pkeys if self['PARTICLEPROP'][x] == 'REAL']
        self['realproperty'].sort()
        if self['realproperty']:
          self['n_real_props'] = len(self['realproperty'])
        else:
          self['n_real_props'] = 1


        listTuples = list(self['SPECIES'].items())
        #Provide a custom compare function to the list's sort method.
        #Sorts list by the number of electrons (field 1) and then alphabetically (field 0).
        #The second condition is only evaluated if the first condition is false.
        try:
            listTuples.sort( key = lambda x : (int(x[1]),x[0]) )
        except: # for Python versions before 2.4, but those probably do not work for us anyway
            listTuples.sort( cmp = lambda x,y : cmp(int(x[1]),int(y[1])) or cmp(x[0],y[0]) )

        totalSpecies = 0
        self['species'] = []        
        for (element, numElectrons) in listTuples:

            #We must sum over all electrons in all species.
            if numElectrons > 1:
                totalSpecies = totalSpecies + (numElectrons + 1)
                lenAppendedNumber = len(str(numElectrons))
            else:
                totalSpecies += 1
                lenAppendedNumber = 0 
            
            #There is an inbuilt 4-character limit in Paramesh which truncates names.
            #Warn the user if they have exceeded this limit.
            #If there is only one ion for a particular element then we append no number.                
#            if (len(element) + lenAppendedNumber) > 4:
#                raise SetupError('Constructed element name for "%s" will exceed 4 character limit' % (element))

            #Place the unique name in the list.
            self['species'].append(element)
            if numElectrons > 1:
                for electron in range(1, numElectrons+1, 1):
                    self['species'].append("%s%d" % (element, electron))

        
        self['nspecies'] = totalSpecies
        self['nmassscalars'] = len(self['MASS_SCALARS'])
        self['massscalars'] = list(self['MASS_SCALARS'].keys())
        self['massscalars'].sort()
        tmpList = [ self['MASS_SCALARS'][var] for var in self['massscalars'] ]
##        print 'tmpList is',tmpList
        self['eosmapin_ms'] = [y for (x,y,z) in tmpList] # list of corresponding EOSMAPINs
        self['eosmapout_ms'] = [z for (x,y,z) in tmpList] # list of Eos_map roles for EOSMAPOUTs
##        print "self['eosmapin_ms'] is", self['eosmapin_ms'] 
##        print "self['eosmapout_ms'] is", self['eosmapout_ms'] 
        msg = list(self['MASS_SCALAR_GROUPS'].keys())
        msg.sort()
##        print 'msg is', msg
        self['massscalars_map'] = {}
        self['massscalars_group_map'] = {}

##        print "self['MASS_SCALARS'].items() is",self['MASS_SCALARS'].items()
        for name,(group,eos1,eos2) in list(self['MASS_SCALARS'].items()):
            if not group: continue
##            print 'group is', group
            self['massscalars_map'][name] = msg.index(group)+1 # index return 0 based, our groups start at 1
            self['massscalars_group_map'][group] = msg.index(group)+1 # index return 0 based, our groups start at 1
        # now masscalars_map is a dictionary mapping scalar to its group number
        # only mass_scalars which need to be renormed appear in this map
        # mass_scalars_group_map maps group names to numbers

        self['nfacevars'] = len(self['FACEVAR'])
        self['facevar'] = list(self['FACEVAR'].keys())
        self['facevar'].sort()
#        self['eos_facevars'] = [ self['FACEVAR'][var] for var in self['facevar'] ] # list of Eos_map roles.
        tmpList = [ self['FACEVAR'][var] for var in self['facevar'] ]
        self['eosmapin_facevars'] = [ x for (x,y) in tmpList ] # list of Eos_map roles.
        self['eosmapout_facevars'] = [ y for (x,y) in tmpList ] # list of Eos_map roles.


        self['flux'] = list(self['FLUX'].keys())
        self['flux'].sort()


        self['scratchvar'] = list(self['SCRATCHVAR'].keys())
        self['scratchvar'].sort()
#        self['eos_scratchvars'] = [ self['SCRATCHVAR'][var] for var in self['scratchvar'] ] # list of Eos_map roles.
        tmpList = [ self['SCRATCHVAR'][var] for var in self['scratchvar'] ]
        self['eosmapin_scratchvars'] = [ x for (x,y) in tmpList ] # list of Eos_map roles.
        self['eosmapout_scratchvars'] = [ y for (x,y) in tmpList ] # list of Eos_map roles.


        self['scratchcentervar'] = list(self['SCRATCHCENTERVAR'].keys())
        self['scratchcentervar'].sort()

        # If MoL is included, add RHS variables to scractch
        self["nmolrhsvars"] = 0
        self["nmolinitvars"] = 0
        self["nmolscratch"] = 0
        self["nmolscratchvars"] = 0
        if GVars.setupVars.get("TimeAdvance").upper() == "MOL":
            self["nmolrhsvars"] = len(self["evolvedvar"])
            self["nmolinitvars"] = len(self["evolvedvar"])

            rhsvars = [var + "_rhs" for var in self["evolvedvar"]]
            initvars = [var + "_init" for var in self["evolvedvar"]]

            molscratch = []

            if self["NRHS"] != None:
                if self["NRHS"] > 0:
                    self["nmolscratchvars"] = self["NRHS"]*len(self["evolvedvar"])
                    self["nmolscratch"] = self["NRHS"]

                    rhswidth = int(math.ceil(math.log10(self["nmolscratch"])))
                    rhsname = "{}_RHS_{:0" + str(rhswidth) + "d}"

                    molscratch = [rhsname.format(v,i) for i in range(self["nmolscratch"]) for v in self["evolvedvar"]]

            for v in rhsvars:
                self['SCRATCHCENTERVAR'][v] = ("NONEXISTENT","NONEXISTENT")

            for v in initvars:
                self['SCRATCHCENTERVAR'][v] = ("NONEXISTENT","NONEXISTENT")

            for v in molscratch:
                self['SCRATCHCENTERVAR'][v] = ("NONEXISTENT","NONEXISTENT")

            self["scratchcentervar"] = rhsvars + initvars + molscratch + self["scratchcentervar"]

        tmpList = [ self['SCRATCHCENTERVAR'][var] for var in self['scratchcentervar'] ]
        self['eosmapin_scratchcentervars'] = [ x for (x,y) in tmpList ] # list of Eos_map roles.
        self['eosmapout_scratchcentervars'] = [ y for (x,y) in tmpList ] # list of Eos_map roles.


        self['scratchfacexvar'] = list(self['SCRATCHFACEXVAR'].keys())
        self['scratchfacexvar'].sort()
        tmpList = [ self['SCRATCHFACEXVAR'][var] for var in self['scratchfacexvar'] ]
        self['eosmapin_scratchfacexvars'] = [ x for (x,y) in tmpList ] # list of Eos_map roles.
        self['eosmapout_scratchfacexvars'] = [ y for (x,y) in tmpList ] # list of Eos_map roles.

        self['scratchfaceyvar'] = list(self['SCRATCHFACEYVAR'].keys())
        self['scratchfaceyvar'].sort()
        tmpList = [ self['SCRATCHFACEYVAR'][var] for var in self['scratchfaceyvar'] ]
        self['eosmapin_scratchfaceyvars'] = [ x for (x,y) in tmpList ] # list of Eos_map roles.
        self['eosmapout_scratchfaceyvars'] = [ y for (x,y) in tmpList ] # list of Eos_map roles.

        self['scratchfacezvar'] = list(self['SCRATCHFACEZVAR'].keys())
        self['scratchfacezvar'].sort()
        tmpList = [ self['SCRATCHFACEZVAR'][var] for var in self['scratchfacezvar'] ]
        self['eosmapin_scratchfacezvars'] = [ x for (x,y) in tmpList ] # list of Eos_map roles.
        self['eosmapout_scratchfacezvars'] = [ y for (x,y) in tmpList ] # list of Eos_map roles.


        self['nvar'] = self['nspecies'] + self['nmassscalars'] + len(self['variable'])
        self['nflux'] = self['nspecies'] + self['nmassscalars'] + len(self['flux'])
        self['max_plot_vars'] = self['nvar'] # + (self['nfacevars']*3) + self['nflux']


        self['particletype'] = []
        self['initmethod'] = []
        self['mapmethod'] = []
        self['advmethod'] = []
        #We create a sorted temporary list "tmpList", which we then copy into
        #a new list "particleList" with passive particles at the start.
        tmpList = list(self['PARTICLETYPE'].items())
        try:
            tmpList.sort( key = lambda x : x[0] )  #Sort by particle type.
        except: # for Python versions before 2.4, but those probably do not work for us anyway
            tmpList.sort( cmp = lambda x,y : cmp(x[0],y[0]) )  #Sort by particle type.

        i = 0
        particleList = []
        passiveKeyword = "passive"
        for (particleType, (initMethod, mapMethod, advMethod)) in tmpList:
            if (particleType.upper() == passiveKeyword.upper()):
                particleList.append(tmpList[i])
                del tmpList[i]
                break
            i = i + 1
        [particleList.append(x) for x in tmpList]

        for (particleType, (initMethod, mapMethod, advMethod)) in particleList:
            self['particletype'].append(particleType.upper())
            self['initmethod'].append(initMethod.upper())
            self['mapmethod'].append(mapMethod.upper())
            self['advmethod'].append(advMethod.upper())


        #build separate lists for the different grid-variable typed particle maps
        pmkeys = list(self['PARTICLEMAP'].keys())

        self['particlemaps_variable'] = []
        self['particlemaps_species'] = []
        self['particlemaps_mscalar'] = []
        self['particlemaps_facex'] = []
        self['particlemaps_facey'] = []
        self['particlemaps_facez'] = []
        self['particlemaps_scratchvar'] = []
        self['particlemaps_scratchcentervar'] = []
        self['particlemaps_scratchfacexvar'] = []
        self['particlemaps_scratchfaceyvar'] = []
        self['particlemaps_scratchfacezvar'] = []

        self['particlemaps_variable'] = [(x, self['PARTICLEMAP'][x][1]) for x in pmkeys if self['PARTICLEMAP'][x][0] == 'VARIABLE']
        self['particlemaps_species'] = [(x, self['PARTICLEMAP'][x][1]) for x in pmkeys if self['PARTICLEMAP'][x][0] == 'SPECIES']
        self['particlemaps_mscalar'] = [(x, self['PARTICLEMAP'][x][1]) for x in pmkeys if self['PARTICLEMAP'][x][0] == 'MASS_SCALAR']
        self['particlemaps_facex'] = [(x, self['PARTICLEMAP'][x][1]) for x in pmkeys if self['PARTICLEMAP'][x][0] == 'FACEX']
        self['particlemaps_facey'] = [(x, self['PARTICLEMAP'][x][1]) for x in pmkeys if self['PARTICLEMAP'][x][0] == 'FACEY']
        self['particlemaps_facez'] = [(x, self['PARTICLEMAP'][x][1]) for x in pmkeys if self['PARTICLEMAP'][x][0] == 'FACEZ']
        self['particlemaps_scratchvar'] = [(x, self['PARTICLEMAP'][x][1]) for x in pmkeys if self['PARTICLEMAP'][x][0] == 'SCRATCHVAR']
        self['particlemaps_scratchcentervar'] = [(x, self['PARTICLEMAP'][x][1]) for x in pmkeys if self['PARTICLEMAP'][x][0] == 'SCRATCHCENTERVAR']
        self['particlemaps_scratchfacexvar'] = [(x, self['PARTICLEMAP'][x][1]) for x in pmkeys if self['PARTICLEMAP'][x][0] == 'SCRATCHFACEXVAR']
        self['particlemaps_scratchfaceyvar'] = [(x, self['PARTICLEMAP'][x][1]) for x in pmkeys if self['PARTICLEMAP'][x][0] == 'SCRATCHFACEYVAR']
        self['particlemaps_scratchfacezvar'] = [(x, self['PARTICLEMAP'][x][1]) for x in pmkeys if self['PARTICLEMAP'][x][0] == 'SCRATCHFACEZVAR']

        self['nparticlemaps_variable'] = len(self['particlemaps_variable'])
        self['nparticlemaps_facex'] = len(self['particlemaps_facex'])
        self['nparticlemaps_facey'] = len(self['particlemaps_facey'])
        self['nparticlemaps_facez'] = len(self['particlemaps_facez'])
        self['nparticlemaps_scratchvar'] = len(self['particlemaps_scratchvar'])
        self['nparticlemaps_scratchcentervar'] = len(self['particlemaps_scratchcentervar'])
        self['nparticlemaps_scratchfacexvar'] = len(self['particlemaps_scratchfacexvar'])
        self['nparticlemaps_scratchfaceyvar'] = len(self['particlemaps_scratchfaceyvar'])
        self['nparticlemaps_scratchfacezvar'] = len(self['particlemaps_scratchfacezvar'])

        #check the consistency of the particle maps
        for part_var,grid_var in self['particlemaps_variable']:
            if (self['realproperty'].count(part_var) != 1):
                raise SetupError('fatal:  bad particle map for %s; no such property' % part_var)
            if (self['variable'].count(grid_var) == 0):
                raise SetupError('fatal:  bad particle map for %s; no such variable %s' % (part_var,grid_var))
            
        for part_var,grid_var in self['particlemaps_species']:
            if (self['realproperty'].count(part_var) != 1):
                raise SetupError('fatal:  bad particle map for %s; no such property' % part_var)
            if (self['species'].count(grid_var) == 0):
                raise SetupError('fatal:  bad particle map for %s; no such species %s' % (part_var,grid_var))
            
        for part_var,grid_var in self['particlemaps_mscalar']:
            if (self['realproperty'].count(part_var) != 1):
                raise SetupError('fatal:  bad particle map for %s; no such property' % part_var)
            if (self['massscalars'].count(grid_var) == 0):
                raise SetupError('fatal:  bad particle map for %s; no such mass scalar %s' % (part_var,grid_var))
            
        for part_var,grid_var in self['particlemaps_facex']:
            if (self['realproperty'].count(part_var) != 1):
                raise SetupError('fatal:  bad particle map for %s; no such property' % part_var)
            if (self['facevar'].count(grid_var) == 0):
                raise SetupError('fatal:  bad particle map for %s; no such facevar %s' % (part_var,grid_var))

        for part_var,grid_var in self['particlemaps_facey']:
            if (self['realproperty'].count(part_var) != 1):
                raise SetupError('fatal:  bad particle map for %s; no such property' % part_var)
            if (self['facevar'].count(grid_var) == 0):
                raise SetupError('fatal:  bad particle map for %s; no such facevar %s' % (part_var,grid_var))

        for part_var,grid_var in self['particlemaps_facez']:
            if (self['realproperty'].count(part_var) != 1):
                raise SetupError('fatal:  bad particle map for %s; no such property' % part_var)
            if (self['facevar'].count(grid_var) == 0):
                raise SetupError('fatal:  bad particle map for %s; no such facevar %s' % (part_var,grid_var))

        for part_var,grid_var in self['particlemaps_scratchvar']:
            if (self['realproperty'].count(part_var) != 1):
                raise SetupError('fatal:  bad particle map for %s; no such property' % part_var)
            if (self['scratchvar'].count(grid_var) == 0):
                raise SetupError('fatal:  bad particle map for %s; no such scratchvar %s' % (part_var,grid_var))

        for part_var,grid_var in self['particlemaps_scratchcentervar']:
            if (self['realproperty'].count(part_var) != 1):
                raise SetupError('fatal:  bad particle map for %s; no such property' % part_var)
            if (self['scratchcentervar'].count(grid_var) == 0):
                raise SetupError('fatal:  bad particle map for %s; no such scratchcentervar %s' % (part_var,grid_var))

        for part_var,grid_var in self['particlemaps_scratchfacexvar']:
            if (self['realproperty'].count(part_var) != 1):
                raise SetupError('fatal:  bad particle map for %s; no such property' % part_var)
            if (self['scratchfacexvar'].count(grid_var) == 0):
                raise SetupError('fatal:  bad particle map for %s; no such scratchfacexvar %s' % (part_var,grid_var))

        for part_var,grid_var in self['particlemaps_scratchfaceyvar']:
            if (self['realproperty'].count(part_var) != 1):
                raise SetupError('fatal:  bad particle map for %s; no such property' % part_var)
            if (self['scratchfaceyvar'].count(grid_var) == 0):
                raise SetupError('fatal:  bad particle map for %s; no such scratchfaceyvar %s' % (part_var,grid_var))

        for part_var,grid_var in self['particlemaps_scratchfacezvar']:
            if (self['realproperty'].count(part_var) != 1):
                raise SetupError('fatal:  bad particle map for %s; no such property' % part_var)
            if (self['scratchfacezvar'].count(grid_var) == 0):
                raise SetupError('fatal:  bad particle map for %s; no such scratchfacezvar %s' % (part_var,grid_var))

# __author__ = 'bz'
import json


userIDMax = 0
exPointMax = 0
PackageMax = 0
RouteMax = 0
PathMax = 0
Users = []
ExPoints = []
Packages = []
Routes = []
Paths = []




class User:
    def __init__(self):
        global userIDMax
        self.UserID = userIDMax
        userIDMax += 1
        self.pathIDs = {}


class ExPoint:
    def __init__(self):
        global exPointMax
        self.exID = exPointMax
        exPointMax += 1
        self.PackageID = []


class Package:
    def __init__(self):
        global PackageMax
        self.PackageID = PackageMax
        PackageMax += 1
        self.Start = 0
        self.End = 0
        self.MaxTime = 0
        self.Travelling = False
        self.RouteID = 0


class Route:
    def __init__(self):
        global RouteMax
        self.RouteID = RouteMax
        RouteMax += 1
        self.exPID = []
        self.UserIDs = []


class Path:
    def __init__(self):
        global PathMax
        self.PathID = PathMax
        PathMax += 1
        self.StartPoint = 0
        self.EndPoint = 0
        self.StartTime = 0
        self.EndTime = 0
        self.exPoints = []



def ParseUsers(Infos):
    parsed = json.loads(Infos)
    for i in range(len(parsed) ):
        user = User()
        user.UserID = parsed[i]['UserID']
        user.pathIDs = parsed[i]['PathIDs']
        Users.append(parsed[i])
    print "read Users"


def ParseRoute(Infos):
    parsed = json.loads(Infos)
    for i in range(len(parsed) ):
        route = Route()
        route.RouteID = parsed[i]['RouteID']
        route.exPID = parsed[i]['ExchangeIDs']
        route.UserIDs = parsed[i]['UserIDs']
        Routes.append(route)
    print "read Route"


def ParsePath(Infos):
    parsed = json.loads(Infos)
    for i in range(len(parsed) ):
        path = Path()
        #for att in parsed
        path.PathID = parsed[i]['PathID']
        path.StartPoint = parsed[i]['StartPoint']
        path.EndPoint = parsed[i]['EndPoint']
        path.StartTime = parsed[i]['StartTime']
        path.EndTime = parsed[i]['EndTime']
        path.exPoints = parsed[i]['exchangePoints']
        Paths.append(path)
    print "raed Path"


def ParsePackage(Infos):
    parsed = json.loads(Infos)
    for i in range(len(parsed) ):
        pack = Package()
        pack.PackageID = parsed[i]['PackageID']
        pack.Start = parsed[i]['Start']
        pack.End = parsed[i]['End']
        pack.MaxTime = parsed[i]['MaxTime']
        pack.Travelling = parsed[i]['Travelling']
        pack.RouteID = parsed[i]['RouteID']
        Packages.append(pack)
    print "read Packages"


def ParsePoints(Infos):
    parsed = json.loads(Infos)
    for i in range(len(parsed) ):
        points = ExPoint()
        points.exID = parsed[i]['ExchangeID']
        points.PackageID = parsed[i]['PackageID']
        ExPoints.append(points)
    print "read Route"



with open("Users.json") as f:
    UserInfos = f.read()
    ParseUsers(UserInfos)

with open("exchangePoints.json") as f:
    exPs = f.read()
    ParsePoints(exPs)

with open("Package.json") as f:
    packages = f.read()
    ParsePackage(packages)

with open("Route.json") as f:
    routes = f.read()
    ParseRoute(routes)

with open("Path.json") as f:
    paths = f.read()
    ParsePath(paths)



def getUser(id):
    global Users
    for user in Users:
        if user.UserID == id:
            return user
    return None


def getExPoint(id):
    global ExPoints
    for exP in ExPoints:
        if exP.exID == id:
            return exP
    return None


def getPackage(id):
    global Packages
    for pack in Packages:
        if pack.PackageID == id:
            return pack
    return None


def getRoute(id):
    global Routes
    for route in Routes:
        if route.RouteID == id:
            return route
    return None


def getPath(id):
    global Paths
    for path in Paths:
        if path.PathID == id:
            return path
    return None
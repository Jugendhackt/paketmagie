# __author__ = 'bz'


userIDMax = 0
exPointMax = 0
PackageMax = 0
RouteMax = 0
PathMax = 0
Users = {}
ExPoints = {}
Packages = {}
Routes = {}
Paths = {}


with open("Users.json") as f:
    UserInfos = f.read()

with open("exchangePoints.json") as f:
    exPs = f.read()

with open("Package.json") as f:
    packages = f.read()

with open("Route.json") as f:
    routes = f.read()

with open("Path.json") as f:
    paths = f.read()



class User:
    def __init__(self):
        self.UserID = userIDMax
        global userIDMax
        userIDMax += 1
        self.pathIDs = {}


class ExPoint:
    def __init__(self):
        self.exID = exPointMax
        global exPointMax
        exPointMax += 1
        self.PackageID = []


class Package:
    def __init__(self):
        self.PackageID = PackageMax
        global PackageMax
        PackageMax += 1
        self.Start = 0
        self.End = 0
        self.MaxTime = 0
        self.Travelling = False
        self.RouteID = 0


class Route:
    def __init__(self):
        self.RouteID = RouteMax
        global RouteMax
        RouteMax += 1
        self.exPID = []
        self.UserIDs = []


class Path:
    def __init__(self):
        self.PathID = PathMax
        self.StartPoint = 0
        self.EndPoint = 0
        self.StartTime = 0
        self.EndTime = 0
        self.exPoints = []


def getUser(id):
    for user in Users:
        if user.UserID == id:
            return user
    return None


def getExPoint(id):
    for exP in ExPoints:
        if exP.exID == id:
            return exP
    return None


def getPackage(id):
    for pack in Packages:
        if pack.PackageID == id:
            return pack
    return None


def getRoute(id):
    for route in Routes:
        if route.RouteID == id:
            return route
    return None


def getPath(id):
    for path in Paths:
        if path.PathID == id:
            return path
    return None
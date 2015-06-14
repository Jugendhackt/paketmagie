# __author__ = 'bz'
import json
from geopy.geocoders import Nominatim
import random

random.seed()

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




class User(object):
    def __init__(self):
        global userIDMax
        self.UserID = userIDMax
        userIDMax += 1
        self.pathIDs = {}


class ExPoint(object):
    def __init__(self):
        global exPointMax
        self.exID = exPointMax
        exPointMax += 1
        self.PackageID = []
        self.Location = [0, 0]
        self.address = ""
        self.Name = ""


class Package(object):
    def __init__(self):
        global PackageMax
        self.PackageID = PackageMax
        PackageMax += 1
        self.Start = 011
        self.End = 0
        self.MaxTime = 0
        self.Travelling = False
        self.RouteID = 0


class Route(object):
    def __init__(self):
        global RouteMax
        self.RouteID = RouteMax
        RouteMax += 1
        self.exPID = []
        self.UserIDs = []


class Path(object):
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
    print "read Path"


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
        points.Location = parsed[i]['Location']
        points.address = parsed[i]['Address']
        points.Name = parsed[i]['Name']
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


def addExPoint(point):
    global ExPoints
    if type(point) == ExPoint:
        ExPoints.append(point)
        point_dict = {
            "ExchangeID": point.exID,
            "Location": point.Location,
            "Name": point.Name,
            "Address": point.address,
            "Volume": [0, 0, 0],
            "PackageID": point.PackageID
        }
        with open("exchangePoints.json", "r+") as f:
            f.seek(-2, 2)
            f.write(',\n')
            print json.dump( point_dict, f, indent=2)
            f.write('\n]')


def addPackage(pack):
    global Packages
    if type(pack) == Package:
        Packages.append(pack)
        pack_dict = {
            "PackageID": pack.PackageID,
            "Start": pack.Start,
            "End": pack.End,
            "MaxTime": pack.MaxTime,
            "Travelling": pack.Travelling,
            "RouteID": pack.RouteID
        }
        with open("Package.json", "r+") as f:
            f.seek(-2, 2)
            f.write(',\n')
            print json.dump( pack_dict, f, indent=2)
            f.write('\n]')


def addPath(path):
    global Path
    if type(path) == Path:
        Paths.append(path)
        point_dict = {
            "PathID": path.PathID,
            "StartPoint": path.StartPoint,
            "EndPoint": path.EndPoint,
            "StartTime": path.StartTime,
            "EndTime": path.EndTime,
            "exchangePoints": path.exPoints
        }
        with open("exchangePoints.json", "r+") as f:
            f.seek(-2, 2)
            f.write(',\n')
            print json.dump( point_dict, f, indent=2)
            f.write('\n]')


def addRoute(route):
    global Routes
    if type(route) == Route:
        Routes.append(route)
        point_dict = {
            "RouteID": route.RouteID,
            "ExchangeIDs": route.exPID,
            "UserIDs": route.UserIDs
        }
        with open("Route.json", "r+") as f:
            f.seek(-2, 2)
            f.write(',\n')
            print json.dump( point_dict, f, indent=2)
            f.write('\n]')


def addUsers(user):
    global Users
    if type(user) == User:
        Users.append(user)
        point_dict = {
            "UserID": user.UserID,
            "PathIDs": user.pathIDs
        }
        with open("exchangePoints.json", "r+") as f:
            f.seek(-2, 2)
            f.write(',\n')
            print json.dump( point_dict, f, indent=2)
            f.write('\n]')


def getStatistics(n):
    stats = []
    for i in range(n):
        stats.append(random.uniform(0.2, 1.0) )
    return stats


def getJsonData(n):
    data = {
        "edges": []
    }
    for i in range(1, len(ExPoints.__sizeof__() ) ):
        point = getExPoint(i)
        for l in range(1, len(ExPoints.__sizeof__() ) ):
            point2 = getExPoint(l)
            jsonfoo = { "to": point.Name, "from": point2.Name,
                        "propabilities": getStatistics(n) }
            data['edges'].append(jsonfoo)
    return data

# print "done, starting geopy"

# name = "Augsburg Moritzplatz"
#
# geolocator = Nominatim()
# location = geolocator.geocode(name)
# print location.address
#
# point = ExPoint()
# point.Location = [location.latitude, location.longitude]
# point.Name = name
# point.address = location.address
# addExPoint(point)
#
# print "added ExPoint"

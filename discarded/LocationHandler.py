# __author__ = 'bz'
import json
import subprocess
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


"""
Defining all the objects needed later for saving infos
"""


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


"""
Defining all the methods parsing
"""


def parse_users(infos):
    parsed = json.loads(infos)
    for i in range(len(parsed)):
        user = User()
        user.UserID = parsed[i]['UserID']
        user.pathIDs = parsed[i]['PathIDs']
        Users.append(parsed[i])
    # print "read Users"


def parse_route(infos):
    parsed = json.loads(infos)
    for i in range(len(parsed)):
        route = Route()
        route.RouteID = parsed[i]['RouteID']
        route.exPID = parsed[i]['ExchangeIDs']
        route.UserIDs = parsed[i]['UserIDs']
        Routes.append(route)
    # print "read Route"


def parse_path(infos):
    parsed = json.loads(infos)
    for i in range(len(parsed)):
        path = Path()
        # for att in parsed
        path.PathID = parsed[i]['PathID']
        path.StartPoint = parsed[i]['StartPoint']
        path.EndPoint = parsed[i]['EndPoint']
        path.StartTime = parsed[i]['StartTime']
        path.EndTime = parsed[i]['EndTime']
        path.exPoints = parsed[i]['exchangePoints']
        Paths.append(path)
    # print "read Path"


def parse_package(infos):
    parsed = json.loads(infos)
    for i in range(len(parsed)):
        pack = Package()
        pack.PackageID = parsed[i]['PackageID']
        pack.Start = parsed[i]['Start']
        pack.End = parsed[i]['End']
        pack.MaxTime = parsed[i]['MaxTime']
        pack.Travelling = parsed[i]['Travelling']
        pack.RouteID = parsed[i]['RouteID']
        Packages.append(pack)
    # print "read Packages"


def parse_points(infos):
    parsed = json.loads(infos)
    for i in range(len(parsed)):
        points = ExPoint()
        points.exID = parsed[i]['ExchangeID']
        points.PackageID = parsed[i]['PackageID']
        points.Location = parsed[i]['Location']
        points.address = parsed[i]['Address']
        points.Name = parsed[i]['Name']
        ExPoints.append(points)
    # print "read Route"


"""
opening the files and Parsing the found json objects
"""

with open("Users.json") as f:
    user_infos = f.read()
    parse_users(user_infos)

with open("exchangePoints.json") as f:
    exPs = f.read()
    parse_points(exPs)

with open("Package.json") as f:
    packages = f.read()
    parse_package(packages)

with open("Route.json") as f:
    routes = f.read()
    parse_route(routes)

with open("Path.json") as f:
    paths = f.read()
    parse_path(paths)


"""
API for accessing the JSON data
"""


def get_user(user_id):
    global Users
    for user in Users:
        if user.UserID == user_id:
            return user
    return None


def get_exchange_point(ex_id):
    global ExPoints
    for exP in ExPoints:
        if exP.exID == ex_id:
            return exP
    return None


def get_package(package_id):
    global Packages
    for pack in Packages:
        if pack.PackageID == package_id:
            return pack
    return None


def get_route(route_id):
    global Routes
    for route in Routes:
        if route.RouteID == route_id:
            return route
    return None


def get_path(path_id):
    global Paths
    for path in Paths:
        if path.PathID == path_id:
            return path
    return None

"""
API for adding data to the JSON files
"""


def add_exchange_point(point):
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
        with open("exchangePoints.json", "r+") as exPoints_json:
            exPoints_json.seek(-2, 2)
            exPoints_json.write(',\n')
            # print
            json.dump(point_dict, exPoints_json, indent=2)
            exPoints_json.write('\n]')


def add_package(pack):
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
        with open("Package.json", "r+") as packs_json:
            packs_json.seek(-2, 2)
            packs_json.write(',\n')
            # print
            json.dump(pack_dict, packs_json, indent=2)
            packs_json.write('\n]')


def add_path(path):
    # global Path
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
        with open("exchangePoints.json", "r+") as paths_json:
            paths_json.seek(-2, 2)
            paths_json.write(',\n')
            # print
            json.dump(point_dict, paths_json, indent=2)
            paths_json.write('\n]')


def add_route(route):
    global Routes
    if type(route) == Route:
        Routes.append(route)
        point_dict = {
            "RouteID": route.RouteID,
            "ExchangeIDs": route.exPID,
            "UserIDs": route.UserIDs
        }
        with open("Route.json", "r+") as routes_json:
            routes_json.seek(-2, 2)
            routes_json.write(',\n')
            # print
            json.dump(point_dict, routes_json, indent=2)
            routes_json.write('\n]')


def add_users(user):
    global Users
    if type(user) == User:
        Users.append(user)
        point_dict = {
            "UserID": user.UserID,
            "PathIDs": user.pathIDs
        }
        with open("exchangePoints.json", "r+") as points:
            points.seek(-2, 2)
            points.write(',\n')
            # print
            json.dump(point_dict, points, indent=2)
            points.write('\n]')

"""
input: a number n
returns: all the routes possible with @param n number probabilities for connections to each other
"""


def get_stats(n):
    stats = []
    for i in range(0, n):
        stats.append(random.uniform(0.0, 1.0))
    return stats


def get_json_data(n):
    data = {
        "edges": []
    }
    for i in range(1, 6):
        point = get_exchange_point(i)
        for l in range(1, 6):
            point2 = get_exchange_point(l)
            if point.exID != point2.exID:
                jsonfoo = {"to": point.Name, "from": point2.Name, "probabilities": get_stats(n)}
                # print jsonfoo
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


# print get_json_data(int(raw_input()))

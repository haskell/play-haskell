#!/usr/bin/env python3
import sys, os, subprocess
from collections import deque

ignored_packages = set([
    # this is the name of the temporary package we're trying to build
    "sandbox",
    # These are the ghc boot libs
    "Cabal-syntax", "Win32", "array", "base", "binary", "bytestring", "containers", "deepseq",
    "directory", "exceptions", "filepath", "ghc", "ghc-bignum", "ghc-boot", "ghc-boot-th",
    "ghc-compact", "ghc-heap", "ghc-prim", "ghci", "haskeline", "hpc", "integer-gmp", "mtl",
    "parsec", "pretty", "process", "random", "rts", "stm", "template-haskell", "terminfo", "text",
    "time", "transformers", "unix", "xhtml"
])

def dep_to_package(dep):
    for i, c in enumerate(dep):
        if c in "<>=":
            dep = dep[:i]
            break

    return dep.strip()

# returns True if successful
# returns (cabal error output, list of packages in the conflict set) if unsuccessful
def check(ghcversion, dependencies):
    result = subprocess.run(["./mkbuildscript.sh", "-t", ghcversion, ", ".join(dependencies)], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    if result.returncode == 0:
        return True
    else:
        output = result.stderr.decode("utf8")
        lines = output.split("\n")
        for i, line in enumerate(lines):
            if line.startswith("After searching the rest of the dependency tree exhaustively"):
                break
        else:
            print("Could not parse cabal error output:", file=sys.stderr)
            print(output, file=sys.stderr)
            sys.exit(1)

        lines = lines[i:]
        lines[0] = lines[0][lines[0].index(":")+1 :]
        conflicts = [s.strip() for s in " ".join(lines).split(",")]
        return (output.strip(), conflicts)

def main():
    os.chdir(os.path.dirname(sys.argv[0]))

    if len(sys.argv) != 3:
        print(f"Usage: {sys.argv[0]} <ghcversion> <dependencies>", file=sys.stderr)
        print("This is a tool to automatically determine a subset of dependencies that", file=sys.stderr)
        print("produce a working cabal configuration with a particular GHC version.", file=sys.stderr)
        sys.exit(1)

    ghcversion = sys.argv[1]
    initdependencies = [s.strip() for s in sys.argv[2].split(",")]

    queue = deque([initdependencies])
    certainly_broken = set()  # packages that are known to individually not work
    successes = []  # dependency sets that led to a successful configure

    def push_deps_without_pkg(deps, pkg):
        nonlocal queue
        for i, dep in enumerate(deps):
            if dep_to_package(dep) == pkg:
                break
        else:
            print(f"Package {pkg} was not found in the dependency list for this run?")
            print(", ".join(deps))
            sys.exit(1)
        queue.append(deps[:i] + deps[i+1:])

    while len(queue) > 0:
        # Remove the freeze file so that the next configure doesn't get impacted by the previous
        try: os.remove(f"builderprojs/ghc-{ghcversion}-proj/cabal.project.freeze")
        except FileNotFoundError: pass

        deps = queue.popleft()
        if len(set(dep_to_package(dep) for dep in deps) & certainly_broken) > 0:
            print(f"<skipping set with {len(deps)} pkgs>")
            continue  # don't try a set that is going to lead to failure anyway

        print(f"<set with {len(deps)} deps>")

        result = check(ghcversion, deps)
        if result is True:
            successes.append(deps)
            print("SUCCESS: {}".format(", ".join(deps)))
        else:
            cabal_output = result[0]
            pkgs_in_deps = [dep_to_package(dep) for dep in deps]
            conflicts = [pkg for pkg in result[1] if pkg not in ignored_packages]

            if len(conflicts) == 0:
                print("This package set: {}".format(", ".join(deps)))
                print("Fails with the following conflict set: {}".format(", ".join(result[1])))
                print("But all of those are presumed-working packages. What's up? Did you 'cabal update'?")
                print("Full cabal error:")
                print(result[0])
                sys.exit(1)

            if len(conflicts) == 1:
                conflict = conflicts[0]
                print(f"Broken: {conflict}")
                certainly_broken.add(conflict)
                push_deps_without_pkg(deps, conflict)
                continue

            dep_conflicts = [pkg for pkg in result[1] if pkg in pkgs_in_deps and pkg not in ignored_packages]

            if len(dep_conflicts) == 0:
                print(result[0])
                print("\nThe full conflict set consists of packages that are either presumed-working")
                print("or not in the dependency list. Please enter the package name that we'll consider")
                print("broken.")
                print("> ", end="")
                pkg = input().strip()
                print(f"Marking '{pkg}' as broken.")
                certainly_broken.add(pkg)
                push_deps_without_pkg(deps, pkg)
                continue

            if len(dep_conflicts) == 1:
                pkg = dep_conflicts[0]
                print(f"Only one conflict set package was in the dependency list: {pkg}")
                print(f"Marking '{pkg}' as broken.")
                certainly_broken.add(pkg)
                push_deps_without_pkg(deps, pkg)
                continue

            print("Cabal error:")
            print(result[0])
            print("\nConflict set (only dependencies):")
            for i, dep in enumerate(dep_conflicts):
                print(f"- [{i+1}] {dep}")
            while True:
                print("Enter space-separated indices of the packages that might be the issue:\n> ", end="")
                inp = input()
                try:
                    chosen_conflicts = [dep_conflicts[int(i) - 1] for i in inp.split()]
                    break
                except Exception:
                    print("Your input didn't validate.")

            print("Selected: {}".format(", ".join(chosen_conflicts)))
            if len(chosen_conflicts) == 1:
                print("Marking {} as a broken package".format(chosen_conflicts[0]))
                certainly_broken.add(chosen_conflicts[0])

            for pkg in chosen_conflicts:
                push_deps_without_pkg(deps, pkg)

    print(successes)

if __name__ == "__main__":
    main()

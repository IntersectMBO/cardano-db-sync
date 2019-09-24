#include <sys/types.h>
#include <pwd.h>
#include <iostream>
#include <unistd.h>
#include <cassert>

using namespace std;

int main (int argc, char **argv) {
  assert(argc >= 3);
  struct passwd *target_user = 0;
  target_user = getpwnam(argv[1]);

  if(setgid(target_user->pw_gid)) {
    cerr << "unable to setgid()" << endl;
    return -2;
  }
  if(setuid(target_user->pw_uid)) {
    cerr << "unable to setuid()" << endl;
    return -2;
  }
  argv++;
  argv++;
  execvp(argv[0], argv);
}

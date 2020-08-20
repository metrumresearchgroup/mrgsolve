########################################
##       START CONFIGURE BLOCK        ##
########################################

# project name
local name = "mrgsolve";

# where to mount a temporary volume; useful for persisting files across steps
local temp_volume_dir = "/ephemeral";

# environment variables available to every R call, including devtools::check();
# R_LIBS_USER will be set separately; use the empty object {} if there are no
# variables to pass
local r_env_vars = {
  "NOT_CRAN": "true",  # you almost certainly want this
  "_MRGSOLVE_SKIP_MODLIB_BUILD_": "false",
};

# images specified as repo:tag; first element will be used to build release
# https://console.aws.amazon.com/ecr/repositories/mpn-dev/?region=us-east-1
# generally, only the second element should be modified
local ci_images = [
  "mpn-dev:latest",      # latest MPN snapshot
  "mpn-dev:2020-03-24",  # oldest compatible snapshot
  "cran-latest:latest",  # latest MPN snapshot for MRG packages + current CRAN
];

# events that should not trigger Drone; recommended value is "promote", see
# https://discourse.drone.io/t/github-pages-triggering-builds-incorrectly/6370
local exclude_events = [
  "promote",
];

########################################
##        END CONFIGURE BLOCK         ##
########################################

# there should generally be no need to modify anything below here

########################################
##        START SETTINGS BLOCK        ##
########################################

# R major.minor versions to test against
# - first element will be used to lint package and build release
# - must be updated when versions in images are updated
local r_versions = [
  "4.0",
  "3.6",
];

local ecr_repo_base = "906087756158.dkr.ecr.us-east-1.amazonaws.com";

local s3_url_base = "s3://mpn.metworx.dev/releases";

local default_git_user    = "Drony";
local default_git_email   = "drone@metrumrg.com";
local default_volume_name = "docker.sock";
local default_volume_path = "/var/run/docker.sock";

########################################
##         END SETTINGS BLOCK         ##
########################################

########################################
##       START UTILITIES BLOCK        ##
########################################

# Convert a key-value pair to a string "key" = "value"
#
# key             a key
# value           a value
local kv_to_string(key, value) = '"' + key + '" = "' + value + '"';

# Concatenate key-value pairs from an object
#
# obj             an object with key-value pairs
local concat_kvs(obj) =
  local kvs = std.mapWithKey(kv_to_string, obj);

  std.join(", ", [kvs[x] for x in std.objectFields(kvs)]);

# Create a build tag
#
# name            name of the application
# image           image name as repo:tag
local create_build_tag(name, image) = std.join("-", [name, image]);

# Create a CI image name
#
# repo            the repository base
# tag             image name as repo:tag
local create_ci_image(repo, image) = std.join("/", [repo, image]);

# Get the name of the environment variable holding the path to the R executable
# r_major_minor   R version to use, as major.minor, e.g., "4.0"
local get_r_exe_var(r_major_minor) =
  std.join("_", ["R", "EXE"] + std.split(r_major_minor, "."));

########################################
##        END UTILITIES BLOCK         ##
########################################

########################################
##        START PIPELINE BLOCK        ##
########################################

# Add a block to control whether Drone executes for certain events
#
# include         array of events that should trigger Drone
# exclude         array of events that should not trigger Drone
local add_trigger(include=[], exclude=[]) = {
  "trigger": {
    "event": {
      "include": include,
      "exclude": exclude,
    }
  }
};

# Create a volume "object"
#
# name            volume name
# path            volume path (mount point)
local volume(name, path) = {
  "name": name,
  "path": path,
};

# Add host volume to pipeline volumes
# Intended to be called from add_volumes()
#
# volume          volume object
local add_host_volume(volume) = {
  "name": volume.name,
  "host": {
    "path": volume.path,
  },
};

# Add temporary volume to pipeline volumes
# Intended to be called from add_volumes()
#
# volume          volume object
local add_temp_volume(volume) = {
  "name": volume.name,
  "temp": {},
};

# Add volumes to a pipeline
#
# host            array of volume objects to be added as host volumes
# temp            array of volume objects to be added as temporary volumes
local add_volumes(host=[], temp=[]) = {
  "volumes":
    [add_host_volume(v) for v in host] +
    [add_temp_volume(v) for v in temp],
};

# Add volume to a step
#
# volume          volume object
local add_step_volume(volume) = {
  "name": volume.name,
  "path": volume.path,
};

# Drone step to pull a Docker image
#
# image           image to pull
# volumes         array of volume objects
local pull_image(image, volumes=[]) = {
  "name": "Pull image",
  "image": "omerxx/drone-ecr-auth",
  "volumes": [add_step_volume(v) for v in volumes],
  "commands": [
      "$(aws ecr get-login --no-include-email --region us-east-1)",
      "docker pull " + image,
  ],
};

# Run an R expression
#
# r_path          path to R executable
# expr            expression to run
local run_r_expression(r_path, expr) =
  std.join(" ", [r_path, "-e", std.escapeStringBash(expr)]);

# Sync to S3
#
# local_path      path to local object
# remote_path     path to S3 location
local s3_sync(local_path, remote_path) = std.format(
  "aws s3 sync %(local)s %(remote)s",
  {
    "local": local_path,
    "remote": remote_path,
  },
);

# Set up a Docker pipeline
#
# name            pipeline name
local setup_docker_pipeline(name) = {
  "kind": "pipeline",
  "type": "docker",
  "name": name,
};

# Shell command to source /etc/environment
local source_env() = ". /etc/environment";

# Drone step to check an R package
#
# r_major_minor   R version to use, as major.minor, e.g., "4.0"
# image_uri       URI of the CI image
# volumes         array of volume objects
local check_step(r_major_minor, image, volumes=[]) = {
  local r_bin_var = "$${" + get_r_exe_var(r_major_minor) + "}",

  "name": "Check package: R " + r_major_minor,
  "image": image,
  "pull": "never",
  "volumes": [add_step_volume(v) for v in volumes],
  "environment": r_env_vars + {
    "R_LIBS_USER": "/opt/rpkgs/" + r_major_minor,
  },
  "commands": [
    # can't evaluate shell expressions in environment
    # https://docs.drone.io/pipeline/environment/syntax/#common-problems
    "export PATH=" + volumes[0].path + ":$PATH",
    source_env(),
    # build the House model
    std.join(" ", [r_bin_var, "-f", "inst/maintenance/build_housemodel.R"]),
    run_r_expression(
      r_bin_var,
      "devtools::install_deps(upgrade = 'never')"
    ),
    # use the build()-check_built() pattern so that we have a tarball that can
    # subsequently be installed
    run_r_expression(
      r_bin_var,
      "devtools::build(path = '.')"
    ),
    run_r_expression(
      r_bin_var,
      std.join(
        "; ",
        [
          "ver <- desc::desc_get('Version')",
          "path <- paste0('mrgsolve_', ver, '.tar.gz')",
          std.join(
            "",
            [
              "devtools::check_built(path, env_vars = c(",
              concat_kvs(r_env_vars),
              "))"
            ]
          ),
        ]
      )
    ),
    # run additional tests
    run_r_expression(
      r_bin_var,
      std.join(
        "; ",
        [
          "ver <- desc::desc_get('Version')",
          "pkg <- paste0('mrgsolve_', ver, '.tar.gz')",
          "install.packages(pkg, repos = NULL)",
          "library(mrgsolve)",
          "testthat::test_dir('inst/maintenance/unit', stop_on_failure = TRUE)",
        ]
      ),
    ),
    run_r_expression(
      r_bin_var, 
      "spelling::spell_check_package()"
    ),
  ],
};

# Drone pipeline to check an R package
#
# name            name of the application
# image           CI image, as repo:tag
local check(name, image) =
  local build_tag = create_build_tag(name, image);
  local image_uri = create_ci_image(ecr_repo_base, image);

  local host_volume = volume(default_volume_name, default_volume_path);
  local temp_volume = volume("cache", temp_volume_dir);

  setup_docker_pipeline(build_tag) +
  add_volumes([host_volume], [temp_volume]) +
  add_trigger(exclude=exclude_events) +
  {
    "steps": [
      pull_image(image_uri, [host_volume]),
    ] + [
      check_step(r_ver, image_uri, [temp_volume])
      for r_ver in r_versions
    ],
  };

# Drone pipeline to lint an R package
# arguments are the same as for check()
local lint(name, r_major_minor, image) =
  local r_bin_var = "$${" + get_r_exe_var(r_major_minor) + "}";

  local image_uri = create_ci_image(ecr_repo_base, image);

  local host_volume = volume(default_volume_name, default_volume_path);

  setup_docker_pipeline(name + "-lint") +
  add_volumes([host_volume]) +
  add_trigger(exclude=exclude_events) +
  {
    "steps": [
      pull_image(image_uri, [host_volume]),
      {
        "name": "Lint package",
        "image": image_uri,
        "pull": "never",
        "environment": r_env_vars + {
          "R_LIBS_USER": "/opt/rpkgs/" + r_major_minor,
        },
        "commands": [
          source_env(),
          # need NOT_CRAN = "true" to run this
          run_r_expression(r_bin_var, "lintr::expect_lint_free()"),
        ],
      },
    ],
  };

# Drone pipeline to build and deploy an R package
# arguments are the same as for check()
local release(name, r_major_minor, image) =
  local r_bin_var = "$${" + get_r_exe_var(r_major_minor) + "}";

  local image_uri = create_ci_image(ecr_repo_base, image);

  local host_volume = volume(default_volume_name, default_volume_path);
  local temp_volume = volume("cache", temp_volume_dir);

  setup_docker_pipeline(name + '-release') +
  add_volumes([host_volume], [temp_volume]) +
  add_trigger(include=["tag"]) +
  {
    "steps": [
      pull_image(image_uri, [host_volume]),
      {
        "name": "Build package",
        "image": image_uri,
        "pull": "never",
        "volumes": [add_step_volume(v) for v in [temp_volume]],
        "environment": r_env_vars + {
          "R_LIBS_USER": "/opt/rpkgs/" + r_major_minor,
        },
        "commands": [
          # git config needs to sit next to pkgpub
          "git config --global user.email " + default_git_email,
          "git config --global user.name " + default_git_user,
          "git fetch --tags",
          "export PATH=" + temp_volume.path + ":$PATH",
          source_env(),
          run_r_expression(
            r_bin_var,
            std.format(
              "pkgpub::create_tagged_repo(.dir = '%s')",
              temp_volume.path
            )
          ),
        ],
      },
      {
        "name": "Publish package",
        "image": image_uri,
        "pull": "never",
        "volumes": [add_step_volume(v) for v in [temp_volume]],
        "commands": [
          s3_sync(
            std.join("/", [temp_volume.path, "${DRONE_TAG}"]),
            std.join("/", [s3_url_base, "${DRONE_REPO_NAME}", "${DRONE_TAG}"]),
          ),
          s3_sync(
            std.join("/", [temp_volume.path, "${DRONE_TAG}"]),
            std.join("/", [s3_url_base, "${DRONE_REPO_NAME}", "latest_tag"]),
          ),
        ],
      },
    ],
  };

########################################
##         END PIPELINE BLOCK         ##
########################################

########################################
##      START DRONE CONFIG BLOCK      ##
########################################

[
  check(name, image)
  for image in ci_images
] + [
  # lint(name, r_versions[0], ci_images[0]),
  # release step requires all check steps to pass
  release(name, r_versions[0], ci_images[0]) +
    {
      "depends_on": [
        create_build_tag(name, image)
        for image in ci_images
      ],
    },
]

########################################
##       END DRONE CONFIG BLOCK       ##
########################################

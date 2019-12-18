#!/usr/bin/python

# This script runs tests for the SPARK provers on Salty examples
import os
import re
import shlex
import subprocess
import sys
import time

# Set the list of examples here
example_list = ['Basic/trafficLight.salt',
                'AnzuSpecs/Arbiter/spec2.salt']

example_list = ['CooperativeSearch/CooperativeSearch.salt',
                'CollisionAvoidance/ctrl.salt',
                'Basic/multiagent_example.salt',
                'Basic/trafficLightEnum.salt',
                'Basic/nested_macro.salt',
                'Basic/play_calling.salt',
                'Basic/init.salt',
                'Basic/middleware.salt',
                'Basic/numeric.salt',
                'Basic/trafficLight.salt',
                'Basic/trafficLightAgg.salt',
                'Basic/maximallyPermissiveTest.salt',
                'TuLiP/ParkingRobot/tulipGr1.salt',
                'TuLiP/GnomesAndTrolls/grid_T1_G2_H4_W6.salt',
                'TuLiP/GnomesAndTrolls/grid_T2_G5_H5_W10.salt',
                'TuLiP/GnomesAndTrolls/grid_T2_G2_H5_W10.salt',
                'TuLiP/GnomesAndTrolls/grid_T1_G2_H5_W10.salt',
                'VIP-Escort/ctrl/vip.salt',
                'test_results/AnzuSpecs/Arbiter/spec2/spec2.salt',
                'Slugs/fastslow_orig.salt',
                'Slugs/networks.salt',
                'Slugs/water_reservoir.salt',
                'Slugs/optimisticRecoveryTest.salt',
                'Slugs/semantics_difference.salt',
                'Slugs/simple_safety_example.salt',
                'LTLMoP/SpiderAndNao.salt',
                'LTLMoP/firefighting.salt',
                'LTLMoP/grocery.salt',
                'LTLMoP/robotWaiter.salt',
                'LTLMoP/linefollower.salt',
                'LTLMoP/meals.salt',
                'LTLMoP/hideandseek.salt',
                'LTLMoP/iRobotCreatewithpickup.salt',
                'AnzuSpecs/running_lights/running_lights_12.salt',
                'AnzuSpecs/running_lights/running_lights_16.salt',
                'AnzuSpecs/running_lights/running_lights_8.salt',
                'AnzuSpecs/running_lights/running_lights_4.salt',
                'AnzuSpecs/genbuf_cfg/spec2.salt',
                'AnzuSpecs/genbuf_cfg/spec1.salt',
                'AnzuSpecs/genbuf_cfg/spec10.salt',
                'AnzuSpecs/genbuf_cfg/spec7.salt',
                'AnzuSpecs/genbuf_cfg/spec5.salt',
                'AnzuSpecs/genbuf_cfg/spec6.salt',
                'AnzuSpecs/genbuf_cfg/spec8.salt',
                'AnzuSpecs/genbuf_cfg/spec4.salt',
                'AnzuSpecs/genbuf_cfg/spec9.salt',
                'AnzuSpecs/genbuf_cfg/spec3.salt',
                'AnzuSpecs/Arbiter/spec2.salt',
                'AnzuSpecs/Arbiter/spec7.salt',
                'AnzuSpecs/Arbiter/spec5.salt',
                'AnzuSpecs/Arbiter/spec6.salt',
                'AnzuSpecs/Arbiter/spec8.salt',
                'AnzuSpecs/Arbiter/spec4.salt',
                'AnzuSpecs/Arbiter/spec9.salt',
                'AnzuSpecs/Arbiter/spec3.salt']

# Set Salty timeout in minutes
salty_timeout = 15

# Set SPARK timeout in minutes
spark_timeout = 30


def main(argv):

    # Create test results directory
    if not os.path.isdir("test_results"):
        os.mkdir("test_results")

    # For each example,
    # (1) Create a directory that mirrors the example directory if necessary
    # (2) - If controller.tgz and results.tgz exist, do nothing
    #     - If controller.tgz does not exist, regenerate and re-run SPARK analysis (throwing out any old analysis)
    #     - If controller.tgz exists, run SPARK analysis
    for example in example_list:
        [input_dir, file_prefix] = example.rsplit('/', 1)
        file_prefix = file_prefix.split('.')[0]
        test_dir = 'test_results/' + input_dir + '/' + file_prefix
        if not os.path.isdir(test_dir):
            os.makedirs(test_dir)

        print('*****************************************************************')

        if os.path.isfile(test_dir + '/controller.tgz') and os.path.isfile(test_dir + '/results.tgz'):
            print("Skipping " + example + ". Controller and results already exist.")
            continue

        salty_file = file_prefix + '.salt'
        if not os.path.isfile(test_dir + '/' + salty_file):
            os.system('cp ' + input_dir + '/' + salty_file + ' ' + test_dir)
        controller_name = get_controller_name(test_dir + '/' + salty_file)

        # Generate controller if it doesn't exist. Otherwise, unpack stored controller
        if not os.path.isfile(test_dir + '/controller.tgz'):
            print("Generating controller for " + example)
            command = 'salty -k ' + salty_file
            try:
                status = subprocess.run(shlex.split(command), cwd=test_dir, timeout=salty_timeout*60)
            except subprocess.TimeoutExpired:
                print("Salty timed out. Continuing to next example")
                continue
            except Exception as e:
                print("Exception type: " + type(e).__name__ + ". Continuing to next example")
            # Create compressed tarball of the controller
            time.sleep(10)
            command = 'tar -czvf controller.tgz ' + controller_name + '.adb ' + controller_name + '.ads ' + salty_file
            subprocess.run(shlex.split(command), cwd=test_dir)
        else:
            print("Unpacking existing controller for " + example)
            command = 'tar -xzvf controller.tgz'
            os.system(command)

        # Always re-run SPARK if you reach this point, removing previous results if they exist
        if os.path.isfile(test_dir + '/results.tgz'):
            os.system('rm ' + test_dir + '/results.tgz')
        make_gnat_project_file(test_dir)
        print("Generating SPARK analysis results for " + example)
        command = 'gnatprove -P default.gpr --level=4 ' + controller_name + '.adb'
        try:
            status = subprocess.run(shlex.split(command), cwd=test_dir, timeout=spark_timeout * 60)
            # Create compressed tarball of the results. Pause is necessary because files take time to update.
            time.sleep(15)
            command = 'tar -czvf results.tgz gnatprove'
            subprocess.run(shlex.split(command), cwd=test_dir)
        except subprocess.TimeoutExpired:
            print("SPARK timed out. Continuing to next example")
        except Exception as e:
            print("Exception type: " + type(e).__name__ + ". Continuing to next example")

        # Remove files
        filename = test_dir + '/' + controller_name + '.adb'
        if os.path.isfile(filename):
            os.system('rm ' + filename)
        filename = test_dir + '/' + controller_name + '.ads'
        if os.path.isfile(filename):
            os.system('rm ' + filename)
        filename = test_dir + '/gnatprove'
        if os.path.isfile(filename):
            os.system('rm -r ' + filename)


def get_controller_name(salty_file):
    with open(salty_file, 'r') as fp:
        salty_spec = fp.read()
        controller_name = (re.findall('controller\s+(\S+)\s+where', salty_spec)[0]).lower()
        return controller_name


def make_gnat_project_file(path):
    with open(path + '/' + 'default.gpr', 'w') as fp:
        fp.write('project Default is\n' +
                 '   for Source_Dirs use (".");\n' +
                 '   for Object_Dir use ".";\n' +
                 '   for Main use ("main.adb");\n' +
                 'end Default;\n')
        return


if __name__ == "__main__":
    main(sys.argv[1:])
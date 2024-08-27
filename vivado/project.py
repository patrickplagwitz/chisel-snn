import os
import re
import shutil
from collections import namedtuple
import subprocess
import json
import contextlib

@contextlib.contextmanager
def cwd(new_dir):
  current_dir = os.getcwd()
  os.chdir(new_dir)
  try:
    yield
  finally:
    os.chdir(current_dir)

def relative_file_path(name):
  current_dir = os.path.dirname(__file__)
  return os.path.join(current_dir, name)

ProjectSources = namedtuple("ProjectSources", ["design", "test_benches",
  "project_file"])

class ImplOptions(object):
  def __init__(self, clk_period_ns, board_name):
    self.clk_period_ns = clk_period_ns
    self.board_name = board_name

def write_constraints_xdc(output, clk_period_ns):
  output.write(
      "create_clock -period {0} -name clock -waveform {{0.000 {1}}} "
      "[get_ports clock]\n".format(clk_period_ns, clk_period_ns / 2))

  set_input_delay_min = "set_input_delay -clock [get_clocks clk] -min " \
      "-add_delay 5.000 "
  set_input_delay_max = "set_input_delay -clock [get_clocks clk] -max " \
      "-add_delay 10.000 "
  set_output_delay = "set_output_delay -clock [get_clocks clk] 5.000 "

#  for input_port in inputs:
#    output.write(set_input_delay_min +
#        get_ports_call(input_port + "[*]") + "\n")
#    output.write(set_input_delay_max +
#        get_ports_call(input_port + "[*]") + "\n")
#    output.write(set_input_delay_min + 
#        get_ports_call(input_port + "_stb") + "\n")
#    output.write(set_input_delay_max + 
#        get_ports_call(input_port + "_stb") + "\n")
#    output.write(set_output_delay +
#        get_ports_call(input_port + "_ack") + "\n")
#
#  for output_port in outputs:
#    output.write(set_output_delay +
#        get_ports_call(output_port + "[*]") + "\n")
#    output.write(set_output_delay +
#        get_ports_call(output_port + "_stb") + "\n")
#    output.write(set_input_delay_min +
#        get_ports_call(output_port + "_ack") + "\n")
#    output.write(set_input_delay_max +
#        get_ports_call(output_port + "_ack") + "\n")
#
#  output.write(set_input_delay_min + get_ports_call("rst") + "\n")
#  output.write(set_input_delay_max + get_ports_call("rst") + "\n")

def write_package_ip_script(out):
  out.write("""
set xprPath [lindex $argv 0]
set repoPath [lindex $argv 1]

open_project $xprPath

#create_bd_design "design_1"
#create_bd_cell -type module -reference hls_fft hls_fft_0

#export_ip_user_files -of_objects [get_files .../*.srcs/sources_1/bd/design_1/design_1.bd] -no_script -reset -force -quiet

ipx::package_project -root_dir $repoPath -vendor FAU -library simulink_blocks -import_files
#-set_current false
#ipx::unload_core /tmp/foo/component.xml
#ipx::edit_ip_in_project -upgrade true -name tmp_edit_project -directory /tmp/foo /tmp/foo/component.xml

foreach input [list {input_names}] {{
  ipx::add_bus_interface $input [ipx::current_core]
  set_property abstraction_type_vlnv xilinx.com:interface:axis_rtl:1.0 [ipx::get_bus_interfaces $input -of_objects [ipx::current_core]]
  set_property bus_type_vlnv xilinx.com:interface:axis:1.0 [ipx::get_bus_interfaces $input -of_objects [ipx::current_core]]
  set_property interface_mode slave [ipx::get_bus_interfaces $input -of_objects [ipx::current_core]]

  ipx::add_port_map TDATA [ipx::get_bus_interfaces $input -of_objects [ipx::current_core]]
  set_property physical_name $input [ipx::get_port_maps TDATA -of_objects [ipx::get_bus_interfaces $input -of_objects [ipx::current_core]]]

  ipx::add_port_map TVALID [ipx::get_bus_interfaces $input -of_objects [ipx::current_core]]
  set_property physical_name ${{input}}_stb [ipx::get_port_maps TVALID -of_objects [ipx::get_bus_interfaces $input -of_objects [ipx::current_core]]]

  ipx::add_port_map TREADY [ipx::get_bus_interfaces $input -of_objects [ipx::current_core]]
  set_property physical_name ${{input}}_ack [ipx::get_port_maps TREADY -of_objects [ipx::get_bus_interfaces $input -of_objects [ipx::current_core]]]

  ipx::associate_bus_interfaces -busif $input -clock clk [ipx::current_core]
}}

foreach output [list {output_names}] {{
  ipx::add_bus_interface $output [ipx::current_core]
  set_property abstraction_type_vlnv xilinx.com:interface:axis_rtl:1.0 [ipx::get_bus_interfaces $output -of_objects [ipx::current_core]]
  set_property bus_type_vlnv xilinx.com:interface:axis:1.0 [ipx::get_bus_interfaces $output -of_objects [ipx::current_core]]
  set_property interface_mode master [ipx::get_bus_interfaces $output -of_objects [ipx::current_core]]

  ipx::add_port_map TDATA [ipx::get_bus_interfaces $output -of_objects [ipx::current_core]]
  set_property physical_name $output [ipx::get_port_maps TDATA -of_objects [ipx::get_bus_interfaces $output -of_objects [ipx::current_core]]]

  ipx::add_port_map TVALID [ipx::get_bus_interfaces $output -of_objects [ipx::current_core]]
  set_property physical_name ${{output}}_stb [ipx::get_port_maps TVALID -of_objects [ipx::get_bus_interfaces $output -of_objects [ipx::current_core]]]

  ipx::add_port_map TREADY [ipx::get_bus_interfaces $output -of_objects [ipx::current_core]]
  set_property physical_name ${{output}}_ack [ipx::get_port_maps TREADY -of_objects [ipx::get_bus_interfaces $output -of_objects [ipx::current_core]]]

  ipx::associate_bus_interfaces -busif $output -clock clk [ipx::current_core]
}}

set_property core_revision 2 [ipx::current_core]
ipx::create_xgui_files [ipx::current_core]
ipx::update_checksums [ipx::current_core]
ipx::save_core [ipx::current_core]

update_ip_catalog
  """.format(input_names=" ".join(["input"]),
      output_names=" ".join(["output"])))

def run_vivado_script(script_path, args):
  subprocess.check_call(["vivado", "-mode", "batch", "-source", script_path,
    "-tclargs"] + args)

class Project(object):
  def __init__(self, directory, project_id, board_name, chip=None):
    self.directory = directory
    self.board_name = board_name
    self.project_id = project_id
    self.chip_name = "Top"

  @property
  def project_file_path(self):
    return os.path.join(self.directory, "project.json")
  @property
  def package_ip_script(self):
    return os.path.join(self.directory, "package-ip.tcl")
  @property
  def vivado_project_dir(self):
    return os.path.join(self.directory, "vivado")
  @property
  def xpr_path(self):
    return os.path.join(self.vivado_project_dir, self.project_id + ".xpr")

  @property
  def src_dir(self):
    return os.path.join(self.directory, "src")

  def generate_vivado_project(self):
    run_vivado_script(relative_file_path("make-project.tcl"),
        [self.directory, self.project_id, self.vivado_project_dir,
          self.board_name])

  def generate_ip_block(self, repo_dir):
    assert os.path.isfile(self.xpr_path)

    run_vivado_script(self.package_ip_script, [self.xpr_path, repo_dir])

  def _get_hdl_files(self, directory):
    for name in os.listdir(directory):
      if name.endswith(".vhd") or name.endswith(".v"):
        yield os.path.join(directory, name)

#  @property
#  def sources(self):
#    return ProjectSources(
#        list(self._get_hdl_files(self.src_dir)),
#        [VerilogTestBench(self.sim_dir)],
#        self.xpr_path)

  def run_implementation(self, results_dir):
    assert os.path.isfile(self.xpr_path)

    run_vivado_script(relative_file_path("synthesize.tcl"), [self.xpr_path])

    impl_dir = os.path.join(self.vivado_project_dir, self.project_id + ".runs",
        "impl_1")

    for report_suffix in [
        "_utilization_placed.rpt", "_timing_summary_routed.rpt",
        "_power_routed.rpt"]:
      shutil.copyfile(
          os.path.join(impl_dir, self.chip_name + report_suffix),
          os.path.join(results_dir, self.project_id + report_suffix))

  @classmethod
  def create(cls, directory, name, impl_opts):
    sim_dir = os.path.join(directory, "sim")
    os.mkdir(sim_dir)
    with open(os.path.join(directory, "constraints.xdc"), "w") as constraints:
      write_constraints_xdc(constraints, impl_opts.clk_period_ns)

    ret = cls(directory, name, impl_opts.board_name)
#    with open(ret.package_ip_script, "w") as package_ip_script:
#      write_package_ip_script(package_ip_script, interface)
    #ret.write_project_file()
    return ret

if __name__ == "__main__":
  import sys
  name = sys.argv[2]
  project = Project.create(sys.argv[1], name, ImplOptions(10, "xczu9eg-ffvb1156-2-e"))
  project.generate_vivado_project()
  project.run_implementation(sys.argv[1])

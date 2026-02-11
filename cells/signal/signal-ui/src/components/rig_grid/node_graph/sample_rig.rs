//! Sample guitar rig factory for [`NodeGraph`].

use signal_control::block::BlockType;

use super::models::{
    GraphModule, Node, NodeGraph, NodePort, NodePosition, NodeSize, NodeWidget, Wire,
};

impl NodeGraph {
    /// Create a comprehensive guitar rig node graph with all modules.
    pub fn sample_guitar_rig() -> Self {
        let mut graph = Self::new();

        let mut y_offset = 100.0;

        // === SOURCE MODULE (contains Guitar Input, Input Gate, Input Volume) ===
        let mut source_module = GraphModule::new(
            "Source",
            BlockType::Input,
            NodePosition::new(50.0, y_offset),
        )
        .with_size(NodeSize::new(300.0, 280.0));

        let input = Node::new(
            "Guitar Input",
            BlockType::Input,
            NodePosition::new(20.0, 50.0),
        )
        .with_size(NodeSize::small())
        .with_short_label("IN");
        let input_id = source_module.add_node(input);

        let input_gate = Node::new("Gate", BlockType::Gate, NodePosition::new(20.0, 140.0))
            .with_size(NodeSize::small());
        let input_gate_id = source_module.add_node(input_gate);

        let input_vol = Node::new("Volume", BlockType::Volume, NodePosition::new(20.0, 210.0))
            .with_size(NodeSize::small());
        let input_vol_id = source_module.add_node(input_vol);

        source_module.add_wire(Wire::new(input_id, "out_l", input_gate_id, "in_l"));
        source_module.add_wire(Wire::new(input_id, "out_r", input_gate_id, "in_r"));
        source_module.add_wire(Wire::new(input_gate_id, "out_l", input_vol_id, "in_l"));
        source_module.add_wire(Wire::new(input_gate_id, "out_r", input_vol_id, "in_r"));

        source_module.auto_size(20.0);
        let source_id = graph.add_module(source_module);

        // === EQ BLOCK ===
        let mut eq_module =
            GraphModule::new("EQ", BlockType::Eq, NodePosition::new(380.0, y_offset));
        let eq = Node::new("EQ", BlockType::Eq, NodePosition::new(10.0, 50.0))
            .with_size(NodeSize::xlarge())
            .with_widget(NodeWidget::EqGraph);
        eq_module.add_node(eq);
        eq_module.auto_size(20.0);
        let eq_id = graph.add_module(eq_module);

        // === DYNAMICS MODULE ===
        let mut dynamics_module = GraphModule::new(
            "Dynamics",
            BlockType::Compressor,
            NodePosition::new(830.0, y_offset),
        );
        let comp = Node::new(
            "Compressor",
            BlockType::Compressor,
            NodePosition::new(10.0, 50.0),
        )
        .with_size(NodeSize::large())
        .with_widget(NodeWidget::CompressorGraph);
        dynamics_module.add_node(comp);
        dynamics_module.auto_size(20.0);
        let dynamics_id = graph.add_module(dynamics_module);

        // === SPECIAL MODULE (Envelope, Wah, Pitch, Doubler) ===
        y_offset += 280.0;
        let mut special_module = GraphModule::new(
            "Special",
            BlockType::Modulation,
            NodePosition::new(50.0, y_offset),
        )
        .with_size(NodeSize::new(950.0, 150.0));

        let envelope = Node::new(
            "Envelope",
            BlockType::Modulation,
            NodePosition::new(20.0, 50.0),
        )
        .with_size(NodeSize::medium());
        let envelope_id = special_module.add_node(envelope);
        let wah = Node::new("Wah", BlockType::Modulation, NodePosition::new(250.0, 50.0))
            .with_size(NodeSize::medium());
        let wah_id = special_module.add_node(wah);
        let pitch = Node::new("Pitch", BlockType::Pitch, NodePosition::new(480.0, 50.0))
            .with_size(NodeSize::medium());
        let pitch_id = special_module.add_node(pitch);
        let doubler = Node::new(
            "Doubler",
            BlockType::Modulation,
            NodePosition::new(710.0, 50.0),
        )
        .with_size(NodeSize::medium());
        let doubler_id = special_module.add_node(doubler);

        special_module.add_wire(Wire::new(envelope_id, "out_l", wah_id, "in_l"));
        special_module.add_wire(Wire::new(envelope_id, "out_r", wah_id, "in_r"));
        special_module.add_wire(Wire::new(wah_id, "out_l", pitch_id, "in_l"));
        special_module.add_wire(Wire::new(wah_id, "out_r", pitch_id, "in_r"));
        special_module.add_wire(Wire::new(pitch_id, "out_l", doubler_id, "in_l"));
        special_module.add_wire(Wire::new(pitch_id, "out_r", doubler_id, "in_r"));

        special_module.auto_size(20.0);
        let special_id = graph.add_module(special_module);

        // === DRIVE MODULE (Boost, Drive 1, Drive 2, Drive 3) ===
        y_offset += 180.0;
        let mut drive_module =
            GraphModule::new("Drive", BlockType::Drive, NodePosition::new(50.0, y_offset))
                .with_size(NodeSize::new(1100.0, 180.0));

        let boost = Node::new("Boost", BlockType::Drive, NodePosition::new(20.0, 60.0))
            .with_size(NodeSize::small());
        let boost_id = drive_module.add_node(boost);
        let drive1 = Node::new("Drive 1", BlockType::Drive, NodePosition::new(200.0, 50.0))
            .with_size(NodeSize::medium())
            .with_widget(NodeWidget::DriveGraph);
        let drive1_id = drive_module.add_node(drive1);
        let drive2 = Node::new("Drive 2", BlockType::Drive, NodePosition::new(450.0, 50.0))
            .with_size(NodeSize::medium())
            .with_widget(NodeWidget::DriveGraph);
        let drive2_id = drive_module.add_node(drive2);
        let drive3 = Node::new("Drive 3", BlockType::Drive, NodePosition::new(700.0, 50.0))
            .with_size(NodeSize::medium())
            .with_widget(NodeWidget::DriveGraph);
        let drive3_id = drive_module.add_node(drive3);

        drive_module.add_wire(Wire::new(boost_id, "out_l", drive1_id, "in_l"));
        drive_module.add_wire(Wire::new(boost_id, "out_r", drive1_id, "in_r"));
        drive_module.add_wire(Wire::new(drive1_id, "out_l", drive2_id, "in_l"));
        drive_module.add_wire(Wire::new(drive1_id, "out_r", drive2_id, "in_r"));
        drive_module.add_wire(Wire::new(drive2_id, "out_l", drive3_id, "in_l"));
        drive_module.add_wire(Wire::new(drive2_id, "out_r", drive3_id, "in_r"));

        drive_module.auto_size(20.0);
        let drive_id = graph.add_module(drive_module);

        // === VOLUME PEDAL ===
        y_offset += 210.0;
        let mut vol_pedal_module = GraphModule::new(
            "Volume",
            BlockType::Volume,
            NodePosition::new(50.0, y_offset),
        );
        let vol_pedal = Node::new("Volume", BlockType::Volume, NodePosition::new(10.0, 50.0))
            .with_size(NodeSize::small());
        vol_pedal_module.add_node(vol_pedal);
        vol_pedal_module.auto_size(20.0);
        let vol_pedal_id = graph.add_module(vol_pedal_module);

        // === PRE-FX MODULE (Pre Delay, Spring Verb) ===
        let mut prefx_module = GraphModule::new(
            "Pre-FX",
            BlockType::Delay,
            NodePosition::new(260.0, y_offset),
        );
        let pre_delay = Node::new("Delay", BlockType::Delay, NodePosition::new(10.0, 50.0))
            .with_size(NodeSize::large())
            .with_widget(NodeWidget::DelayGraph);
        let pre_delay_id = prefx_module.add_node(pre_delay);
        let spring_verb = Node::new("Spring", BlockType::Reverb, NodePosition::new(350.0, 50.0))
            .with_size(NodeSize::large())
            .with_widget(NodeWidget::ReverbGraph);
        let spring_verb_id = prefx_module.add_node(spring_verb);

        prefx_module.add_wire(Wire::new(pre_delay_id, "out_l", spring_verb_id, "in_l"));
        prefx_module.add_wire(Wire::new(pre_delay_id, "out_r", spring_verb_id, "in_r"));
        prefx_module.auto_size(20.0);
        let prefx_id = graph.add_module(prefx_module);

        // === AMP/CAB MODULE (2 Amps, 2 Cabinets, Room Send) ===
        y_offset += 230.0;
        let mut ampcab_module =
            GraphModule::new("Amp/Cab", BlockType::Amp, NodePosition::new(50.0, y_offset))
                .with_size(NodeSize::new(870.0, 300.0));

        let amp1 = Node::new("Amp 1", BlockType::Amp, NodePosition::new(20.0, 50.0))
            .with_size(NodeSize::medium())
            .with_widget(NodeWidget::AmpCab);
        let amp1_id = ampcab_module.add_node(amp1);
        let cab1 = Node::new("Cab 1", BlockType::Cabinet, NodePosition::new(260.0, 50.0))
            .with_size(NodeSize::medium())
            .with_widget(NodeWidget::AmpCab);
        let cab1_id = ampcab_module.add_node(cab1);
        let amp2 = Node::new("Amp 2", BlockType::Amp, NodePosition::new(20.0, 170.0))
            .with_size(NodeSize::medium())
            .with_widget(NodeWidget::AmpCab);
        let amp2_id = ampcab_module.add_node(amp2);
        let cab2 = Node::new("Cab 2", BlockType::Cabinet, NodePosition::new(260.0, 170.0))
            .with_size(NodeSize::medium())
            .with_widget(NodeWidget::AmpCab);
        let cab2_id = ampcab_module.add_node(cab2);
        let room_send = Node::new("Room", BlockType::Send, NodePosition::new(510.0, 110.0))
            .with_size(NodeSize::medium());
        let room_send_id = ampcab_module.add_node(room_send);

        ampcab_module.add_wire(Wire::new(amp1_id, "out_l", cab1_id, "in_l"));
        ampcab_module.add_wire(Wire::new(amp1_id, "out_r", cab1_id, "in_r"));
        ampcab_module.add_wire(Wire::new(cab1_id, "out_l", room_send_id, "in_l"));
        ampcab_module.add_wire(Wire::new(cab1_id, "out_r", room_send_id, "in_r"));
        ampcab_module.add_wire(Wire::new(amp2_id, "out_l", cab2_id, "in_l"));
        ampcab_module.add_wire(Wire::new(amp2_id, "out_r", cab2_id, "in_r"));
        ampcab_module.add_wire(Wire::new(cab2_id, "out_l", room_send_id, "in_l"));
        ampcab_module.add_wire(Wire::new(cab2_id, "out_r", room_send_id, "in_r"));

        ampcab_module.auto_size(20.0);
        let ampcab_id = graph.add_module(ampcab_module);

        // === POST EQ ===
        y_offset += 210.0;
        let mut post_eq_module =
            GraphModule::new("Post EQ", BlockType::Eq, NodePosition::new(50.0, y_offset));
        let post_eq = Node::new("EQ", BlockType::Eq, NodePosition::new(10.0, 50.0))
            .with_size(NodeSize::xlarge())
            .with_widget(NodeWidget::EqGraph);
        post_eq_module.add_node(post_eq);
        post_eq_module.auto_size(20.0);
        let post_eq_id = graph.add_module(post_eq_module);

        // === MODULATION MODULE (Chorus, Flanger, Phaser) ===
        y_offset += 260.0;
        let mut mod_module = GraphModule::new(
            "Modulation",
            BlockType::Modulation,
            NodePosition::new(50.0, y_offset),
        )
        .with_size(NodeSize::new(730.0, 160.0));

        let chorus = Node::new(
            "Chorus",
            BlockType::Modulation,
            NodePosition::new(20.0, 50.0),
        )
        .with_size(NodeSize::medium())
        .with_widget(NodeWidget::ModulationGraph);
        let chorus_id = mod_module.add_node(chorus);
        let flanger = Node::new(
            "Flanger",
            BlockType::Modulation,
            NodePosition::new(260.0, 50.0),
        )
        .with_size(NodeSize::medium())
        .with_widget(NodeWidget::ModulationGraph);
        let flanger_id = mod_module.add_node(flanger);
        let phaser = Node::new(
            "Phaser",
            BlockType::Modulation,
            NodePosition::new(500.0, 50.0),
        )
        .with_size(NodeSize::medium())
        .with_widget(NodeWidget::ModulationGraph);
        let phaser_id = mod_module.add_node(phaser);

        mod_module.add_wire(Wire::new(chorus_id, "out_l", flanger_id, "in_l"));
        mod_module.add_wire(Wire::new(chorus_id, "out_r", flanger_id, "in_r"));
        mod_module.add_wire(Wire::new(flanger_id, "out_l", phaser_id, "in_l"));
        mod_module.add_wire(Wire::new(flanger_id, "out_r", phaser_id, "in_r"));
        mod_module.auto_size(20.0);
        let mod_id = graph.add_module(mod_module);

        // === TIME MODULE (Delay, Reverb, Freeze) ===
        y_offset += 190.0;
        let mut time_module =
            GraphModule::new("Time", BlockType::Delay, NodePosition::new(50.0, y_offset));

        let delay = Node::new("Delay", BlockType::Delay, NodePosition::new(20.0, 50.0))
            .with_size(NodeSize::large())
            .with_widget(NodeWidget::DelayGraph);
        let delay_id = time_module.add_node(delay);
        let reverb = Node::new("Reverb", BlockType::Reverb, NodePosition::new(360.0, 50.0))
            .with_size(NodeSize::large())
            .with_widget(NodeWidget::ReverbGraph);
        let reverb_id = time_module.add_node(reverb);
        let freeze = Node::new("Freeze", BlockType::Freeze, NodePosition::new(710.0, 60.0))
            .with_size(NodeSize::medium());
        let freeze_id = time_module.add_node(freeze);

        time_module.add_wire(Wire::new(delay_id, "out_l", reverb_id, "in_l"));
        time_module.add_wire(Wire::new(delay_id, "out_r", reverb_id, "in_r"));
        time_module.add_wire(Wire::new(reverb_id, "out_l", freeze_id, "in_l"));
        time_module.add_wire(Wire::new(reverb_id, "out_r", freeze_id, "in_r"));
        time_module.auto_size(20.0);
        let time_id = graph.add_module(time_module);

        // === MOTION MODULE (Tremolo, Vibrato, Rotary) ===
        y_offset += 230.0;
        let mut motion_module = GraphModule::new(
            "Motion",
            BlockType::Tremolo,
            NodePosition::new(50.0, y_offset),
        )
        .with_size(NodeSize::new(730.0, 160.0));

        let tremolo = Node::new("Tremolo", BlockType::Tremolo, NodePosition::new(20.0, 50.0))
            .with_size(NodeSize::medium())
            .with_widget(NodeWidget::ModulationGraph);
        let tremolo_id = motion_module.add_node(tremolo);
        let vibrato = Node::new(
            "Vibrato",
            BlockType::Modulation,
            NodePosition::new(260.0, 50.0),
        )
        .with_size(NodeSize::medium());
        let vibrato_id = motion_module.add_node(vibrato);
        let rotary = Node::new(
            "Rotary",
            BlockType::Modulation,
            NodePosition::new(500.0, 50.0),
        )
        .with_size(NodeSize::medium());
        let rotary_id = motion_module.add_node(rotary);

        motion_module.add_wire(Wire::new(tremolo_id, "out_l", vibrato_id, "in_l"));
        motion_module.add_wire(Wire::new(tremolo_id, "out_r", vibrato_id, "in_r"));
        motion_module.add_wire(Wire::new(vibrato_id, "out_l", rotary_id, "in_l"));
        motion_module.add_wire(Wire::new(vibrato_id, "out_r", rotary_id, "in_r"));
        motion_module.auto_size(20.0);
        let motion_id = graph.add_module(motion_module);

        // === MASTER MODULE (Master EQ, Multiband Comp, Output) ===
        y_offset += 190.0;
        let mut master_module =
            GraphModule::new("Master", BlockType::Eq, NodePosition::new(50.0, y_offset));

        let master_eq = Node::new("Master EQ", BlockType::Eq, NodePosition::new(20.0, 50.0))
            .with_size(NodeSize::xlarge())
            .with_widget(NodeWidget::EqGraph);
        let master_eq_id = master_module.add_node(master_eq);
        let master_comp = Node::new(
            "Multiband",
            BlockType::Compressor,
            NodePosition::new(440.0, 50.0),
        )
        .with_size(NodeSize::large())
        .with_widget(NodeWidget::CompressorGraph);
        let master_comp_id = master_module.add_node(master_comp);
        let output = Node::new("Output", BlockType::Volume, NodePosition::new(790.0, 90.0))
            .with_size(NodeSize::small())
            .with_short_label("OUT")
            .with_ports(
                vec![NodePort::input("in_l", "L"), NodePort::input("in_r", "R")],
                vec![],
            );
        let output_id = master_module.add_node(output);

        master_module.add_wire(Wire::new(master_eq_id, "out_l", master_comp_id, "in_l"));
        master_module.add_wire(Wire::new(master_eq_id, "out_r", master_comp_id, "in_r"));
        master_module.add_wire(Wire::new(master_comp_id, "out_l", output_id, "in_l"));
        master_module.add_wire(Wire::new(master_comp_id, "out_r", output_id, "in_r"));
        master_module.auto_size(20.0);
        let master_id = graph.add_module(master_module);

        // === INTER-MODULE SIGNAL CHAIN ===
        graph.connect(source_id, "out_l", eq_id, "in_l");
        graph.connect(source_id, "out_r", eq_id, "in_r");
        graph.connect(eq_id, "out_l", dynamics_id, "in_l");
        graph.connect(eq_id, "out_r", dynamics_id, "in_r");
        graph.connect(dynamics_id, "out_l", drive_id, "in_l");
        graph.connect(dynamics_id, "out_r", drive_id, "in_r");
        graph.connect(drive_id, "out_l", vol_pedal_id, "in_l");
        graph.connect(drive_id, "out_r", vol_pedal_id, "in_r");
        graph.connect(vol_pedal_id, "out_l", prefx_id, "in_l");
        graph.connect(vol_pedal_id, "out_r", prefx_id, "in_r");
        graph.connect(prefx_id, "out_l", ampcab_id, "in_l");
        graph.connect(prefx_id, "out_r", ampcab_id, "in_r");
        graph.connect(ampcab_id, "out_l", post_eq_id, "in_l");
        graph.connect(ampcab_id, "out_r", post_eq_id, "in_r");
        graph.connect(post_eq_id, "out_l", mod_id, "in_l");
        graph.connect(post_eq_id, "out_r", mod_id, "in_r");
        graph.connect(mod_id, "out_l", time_id, "in_l");
        graph.connect(mod_id, "out_r", time_id, "in_r");
        graph.connect(time_id, "out_l", motion_id, "in_l");
        graph.connect(time_id, "out_r", motion_id, "in_r");
        graph.connect(motion_id, "out_l", master_id, "in_l");
        graph.connect(motion_id, "out_r", master_id, "in_r");

        graph
    }
}

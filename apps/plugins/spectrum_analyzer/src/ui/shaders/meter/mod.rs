pub mod pipeline;

use pipeline::MeterPipeline;

use crate::audio::meter::MeterConsumer;
use nih_plug_iced::renderer::wgpu::wgpu;
use nih_plug_iced::widget::shader::{self, Primitive};
use nih_plug_iced::{mouse, Rectangle};

/// MeterShader implements the Program trait for rendering stereo level meters
/// It reads smoothed meter data from MeterConsumer and renders via GPU
pub struct MeterShader {
    meter_consumer: MeterConsumer,
}

impl MeterShader {
    pub fn new(meter_consumer: MeterConsumer) -> Self {
        Self { meter_consumer }
    }
}

impl<Message> shader::Program<Message> for MeterShader {
    type State = ();
    type Primitive = MeterPrimitive;

    fn draw(
        &self,
        _state: &Self::State,
        _cursor: mouse::Cursor,
        bounds: Rectangle,
    ) -> Self::Primitive {
        // Get smoothed meter levels from consumer
        let (left_db, right_db) = self.meter_consumer.get_smoothed_levels_or_silence();

        MeterPrimitive::new(bounds, left_db, right_db)
    }
}

/// MeterPrimitive holds the data needed for one frame of meter rendering
#[derive(Debug)]
pub struct MeterPrimitive {
    bounds: Rectangle,
    left_db: f32,
    right_db: f32,
}

impl MeterPrimitive {
    pub fn new(bounds: Rectangle, left_db: f32, right_db: f32) -> Self {
        Self {
            bounds,
            left_db,
            right_db,
        }
    }
}

impl Primitive for MeterPrimitive {
    type Pipeline = MeterPipeline;

    fn prepare(
        &self,
        pipeline: &mut Self::Pipeline,
        _device: &wgpu::Device,
        queue: &wgpu::Queue,
        _bounds: &Rectangle,
        viewport: &nih_plug_iced::graphics::Viewport,
    ) {
        let physical_size = viewport.physical_size();
        pipeline.update(
            queue,
            &self.bounds,
            physical_size,
            self.left_db,
            self.right_db,
        );
    }

    fn render(
        &self,
        pipeline: &Self::Pipeline,
        encoder: &mut wgpu::CommandEncoder,
        target: &wgpu::TextureView,
        clip_bounds: &Rectangle<u32>,
    ) {
        pipeline.render(encoder, target, *clip_bounds);
    }
}

impl Default for MeterShader {
    fn default() -> Self {
        panic!("MeterShader requires a MeterConsumer - use MeterShader::new() instead")
    }
}

module bitcoin_hash (input  logic clk, reset_n, start,
                     input  logic [15:0] message_addr, output_addr,
                     input  logic [31:0] mem_read_data,
                     output logic done, mem_clk, mem_we,
                     output logic [15:0] mem_addr,
                     output logic [31:0] mem_write_data);

parameter num_nonces = 16;

enum logic [3:0] {IDLE, WAIT, READ, PREPHASE1, PHASE1, PREPHASE2, PHASE2, PREPHASE3, PHASE3, WRITE} state;

logic [31:0] w_common[16];
logic [31:0] w[num_nonces][16];
logic [31:0] w2[num_nonces][16];
logic [15:0] offset; // in word address
logic        cur_we;
logic [15:0] cur_addr;
logic [31:0] cur_write_data;
logic [4:0] index;
logic [4:0] nonce_index;

logic [31:0] h0, h1, h2, h3, h4, h5, h6, h7;
logic [31:0] h0_t, h1_t, h2_t, h3_t, h4_t, h5_t, h6_t, h7_t;
logic [31:0] temp_h[num_nonces][8]; // h0..h7 for nonces 0..15
logic [31:0] h_final[num_nonces][8];

logic [31:0] h_wire[num_nonces][8];
logic [31:0] w_wire[num_nonces][16];

logic reset_module;
logic start_common;
logic done_common;
logic start_sha;
logic [15:0] done_sha;

parameter int k[64] = '{
  32'h428a2f98,32'h71374491,32'hb5c0fbcf,32'he9b5dba5,32'h3956c25b,32'h59f111f1,32'h923f82a4,32'hab1c5ed5,
  32'hd807aa98,32'h12835b01,32'h243185be,32'h550c7dc3,32'h72be5d74,32'h80deb1fe,32'h9bdc06a7,32'hc19bf174,
  32'he49b69c1,32'hefbe4786,32'h0fc19dc6,32'h240ca1cc,32'h2de92c6f,32'h4a7484aa,32'h5cb0a9dc,32'h76f988da,
  32'h983e5152,32'ha831c66d,32'hb00327c8,32'hbf597fc7,32'hc6e00bf3,32'hd5a79147,32'h06ca6351,32'h14292967,
  32'h27b70a85,32'h2e1b2138,32'h4d2c6dfc,32'h53380d13,32'h650a7354,32'h766a0abb,32'h81c2c92e,32'h92722c85,
  32'ha2bfe8a1,32'ha81a664b,32'hc24b8b70,32'hc76c51a3,32'hd192e819,32'hd6990624,32'hf40e3585,32'h106aa070,
  32'h19a4c116,32'h1e376c08,32'h2748774c,32'h34b0bcb5,32'h391c0cb3,32'h4ed8aa4a,32'h5b9cca4f,32'h682e6ff3,
  32'h748f82ee,32'h78a5636f,32'h84c87814,32'h8cc70208,32'h90befffa,32'ha4506ceb,32'hbef9a3f7,32'hc67178f2
};

assign mem_clk = clk;
assign mem_addr = cur_addr + offset;
assign mem_we = cur_we;
assign mem_write_data = cur_write_data;

always_ff @(posedge clk, negedge reset_n)
begin
  if (!reset_n) begin
    cur_we <= 1'b0;
    state <= IDLE;
  end
  else case (state)
    IDLE: begin 
      if(start) begin
        h0 <= 32'h6a09e667;
        h1 <= 32'hbb67ae85;
        h2 <= 32'h3c6ef372;
        h3 <= 32'ha54ff53a;
        h4 <= 32'h510e527f;
        h5 <= 32'h9b05688c;
        h6 <= 32'h1f83d9ab;
        h7 <= 32'h5be0cd19;

        start_common <= 0;
        start_sha <= 0;
        reset_module <= 0;
        cur_addr <= message_addr;
        cur_we <= 0;
        offset <= 0;
        state <= WAIT;
      end
      else
        state <= IDLE;
    end

    WAIT: begin
      reset_module <= 1;
      state <= READ;
    end

    READ: begin
      if(offset < 16) begin
        w_common[offset] <= mem_read_data;
        offset <= offset + 1;
        state <= WAIT;
      end
      else if(offset < 19) begin
        for(nonce_index = 0; nonce_index < num_nonces; nonce_index++)
          w[nonce_index][offset - 16] <= mem_read_data;

        offset <= offset + 1;
        state <= WAIT;
      end
      else begin
        for(nonce_index = 0; nonce_index < num_nonces; nonce_index++) begin
          w[nonce_index][3] <= nonce_index;
          w[nonce_index][4] <= 32'h80000000;
          w[nonce_index][15] <= 640;

          for(index = 5; index < 15; index++)
            w[nonce_index][index] <= 0;
        end

        index <= 0;
        start_common <= 1;
        offset <= 0;
        state <= PREPHASE1;
      end
    end

    PREPHASE1: begin
      if(index == 3) begin
        index <= 0;
        state <= PHASE1;
      end
      else
        index <= index + 1;
    end

    PHASE1: begin
      start_common <= 0;
      if(done_common == 1) begin
        for(nonce_index = 0; nonce_index < num_nonces; nonce_index++) begin
          temp_h[nonce_index][0] <= h0_t;
          temp_h[nonce_index][1] <= h1_t;
          temp_h[nonce_index][2] <= h2_t;
          temp_h[nonce_index][3] <= h3_t;
          temp_h[nonce_index][4] <= h4_t;
          temp_h[nonce_index][5] <= h5_t;
          temp_h[nonce_index][6] <= h6_t;
          temp_h[nonce_index][7] <= h7_t;
        end

        index <= 0;
        start_sha <= 1;
        state <= PREPHASE2;
      end
      else
        state <= PHASE1;
    end

    PREPHASE2: begin
      if(index == 3) begin
        index <= 0;
        start_sha <= 0;
        state <= PHASE2;
      end
      else
        index <= index + 1;
    end

    PHASE2: begin
      if(&done_sha) begin
        for(nonce_index = 0; nonce_index < num_nonces; nonce_index++) begin
          for(index = 0; index < 8; index++)
            w2[nonce_index][index] <= h_final[nonce_index][index];

          w2[nonce_index][8] <= 32'h80000000;
          for(index = 9; index < 15; index++)
            w2[nonce_index][index] <= 'b0;

          w2[nonce_index][15] <= 'd256;
        end
        
        index <= 0;
        start_sha <= 1;
        state <= PREPHASE3;
      end
      else
        state <= PHASE2;
    end

    PREPHASE3: begin
      if(index == 3) begin
        index <= 0;
        start_sha <= 0;
        state <= PHASE3;
      end
      else
        index <= index + 1;
    end

    PHASE3: begin
      if(&done_sha) begin
        cur_addr <= output_addr;
        cur_we <= 1;
        cur_write_data <= h_final[0][0];
        state <= WRITE;
      end
      else
        state <= PHASE3;
    end

    WRITE: begin
      if(offset < 16) begin
        cur_write_data <= h_final[offset+1][0];
			  offset <= offset + 1;
      end
      else begin
        state <= IDLE;
      end
    end

    default: begin
      start_common <= 0;
      start_sha <= 0;
      reset_module <= 0;
      cur_we <= 0;
      offset <= 0;
      state <= IDLE;
    end
  endcase
end

simplified_sha256 sha256_common(
  .clk(clk), .reset_n(reset_module), .start(start_common),
  .read_data(w_common),
  .h0_i(h0), .h1_i(h1), .h2_i(h2), .h3_i(h3), .h4_i(h4), .h5_i(h5), .h6_i(h6), .h7_i(h7),
  .h0_o(h0_t), .h1_o(h1_t), .h2_o(h2_t), .h3_o(h3_t), .h4_o(h4_t), .h5_o(h5_t), .h6_o(h6_t), .h7_o(h7_t),
  .done(done_common)
);

always_comb begin
  for(int ind = 0; ind < num_nonces; ind++) begin
    h_wire[ind][0] = (state == PREPHASE2 || state == PHASE2) ? temp_h[ind][0] : h0;
    h_wire[ind][1] = (state == PREPHASE2 || state == PHASE2) ? temp_h[ind][1] : h1;
    h_wire[ind][2] = (state == PREPHASE2 || state == PHASE2) ? temp_h[ind][2] : h2;
    h_wire[ind][3] = (state == PREPHASE2 || state == PHASE2) ? temp_h[ind][3] : h3;
    h_wire[ind][4] = (state == PREPHASE2 || state == PHASE2) ? temp_h[ind][4] : h4;
    h_wire[ind][5] = (state == PREPHASE2 || state == PHASE2) ? temp_h[ind][5] : h5;
    h_wire[ind][6] = (state == PREPHASE2 || state == PHASE2) ? temp_h[ind][6] : h6;
    h_wire[ind][7] = (state == PREPHASE2 || state == PHASE2) ? temp_h[ind][7] : h7;
    w_wire[ind] = (state == PREPHASE2 || state == PHASE2) ? w[ind] : w2[ind];
  end
end

genvar i1;
generate
  for(i1 = 0; i1 < num_nonces; i1 = i1 + 1) begin: phase2_sha256
    simplified_sha256 sha256_common(
      .clk(clk), .reset_n(reset_module), .start(start_sha),
      .read_data(w_wire[i1]),
      .h0_i(h_wire[i1][0]), .h1_i(h_wire[i1][1]), .h2_i(h_wire[i1][2]), .h3_i(h_wire[i1][3]), 
      .h4_i(h_wire[i1][4]), .h5_i(h_wire[i1][5]), .h6_i(h_wire[i1][6]), .h7_i(h_wire[i1][7]),
      .h0_o(h_final[i1][0]), .h1_o(h_final[i1][1]), .h2_o(h_final[i1][2]), .h3_o(h_final[i1][3]), 
      .h4_o(h_final[i1][4]), .h5_o(h_final[i1][5]), .h6_o(h_final[i1][6]), .h7_o(h_final[i1][7]),
      .done(done_sha[i1])
    );
  end: phase2_sha256
endgenerate

assign done = (state == IDLE);

endmodule
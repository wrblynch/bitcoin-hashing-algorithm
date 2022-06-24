module simplified_sha256 (
  input logic  clk, reset_n, start,
  input logic [31:0] read_data[16],
  input logic [31:0] h0_i, h1_i, h2_i, h3_i, h4_i, h5_i, h6_i, h7_i,
  output logic [31:0] h0_o, h1_o, h2_o, h3_o, h4_o, h5_o, h6_o, h7_o,
  output logic done
);

// FSM state variables 
enum logic [1:0] {IDLE, COMPUTE} state;

// Local variables
logic [31:0] w[16]; // compute
logic [31:0] a, b, c, d, e, f, g, h;
logic [7:0] j;
logic [31:0] s0, s1;
logic [4:0] index;

// SHA256 K constants
parameter int k[0:63] = '{
  32'h428a2f98,32'h71374491,32'hb5c0fbcf,32'he9b5dba5,32'h3956c25b,32'h59f111f1,32'h923f82a4,32'hab1c5ed5,
  32'hd807aa98,32'h12835b01,32'h243185be,32'h550c7dc3,32'h72be5d74,32'h80deb1fe,32'h9bdc06a7,32'hc19bf174,
  32'he49b69c1,32'hefbe4786,32'h0fc19dc6,32'h240ca1cc,32'h2de92c6f,32'h4a7484aa,32'h5cb0a9dc,32'h76f988da,
  32'h983e5152,32'ha831c66d,32'hb00327c8,32'hbf597fc7,32'hc6e00bf3,32'hd5a79147,32'h06ca6351,32'h14292967,
  32'h27b70a85,32'h2e1b2138,32'h4d2c6dfc,32'h53380d13,32'h650a7354,32'h766a0abb,32'h81c2c92e,32'h92722c85,
  32'ha2bfe8a1,32'ha81a664b,32'hc24b8b70,32'hc76c51a3,32'hd192e819,32'hd6990624,32'hf40e3585,32'h106aa070,
  32'h19a4c116,32'h1e376c08,32'h2748774c,32'h34b0bcb5,32'h391c0cb3,32'h4ed8aa4a,32'h5b9cca4f,32'h682e6ff3,
  32'h748f82ee,32'h78a5636f,32'h84c87814,32'h8cc70208,32'h90befffa,32'ha4506ceb,32'hbef9a3f7,32'hc67178f2
};

// SHA256 hash round
function logic [255:0] sha256_op(input logic [31:0] a, b, c, d, e, f, g, h, w,
                                 input logic [7:0] t);
  logic [31:0] S1, S0, ch, maj, t1, t2; // internal signals
  begin
    S1 = rightrotate(e, 6) ^ rightrotate(e, 11) ^ rightrotate(e, 25);
    // Student to add remaning code below
    ch = (e & f) ^ ((~e) & g);
    t1 = h + S1 + ch + k[t] + w;
    S0 = rightrotate(a, 2) ^ rightrotate(a, 13) ^ rightrotate(a, 22);
    maj = (a & b) ^ (a & c) ^ (b & c);
    t2 = S0 + maj;
    sha256_op = {t1 + t2, a, b, c, d + t1, e, f, g};
  end
endfunction

function logic [31:0] w_next;
  begin
    s0 = rightrotate(w[1], 7) ^ rightrotate(w[1], 18) ^ (w[1] >> 3);
    s1 = rightrotate(w[14], 17) ^ rightrotate(w[14], 19) ^ (w[14] >> 10);
    w_next = w[0] + s0 + w[9] + s1;
  end
endfunction	

// Right rotation function
function logic [31:0] rightrotate(input logic [31:0] x,
                                  input logic [ 7:0] r);
  begin
    rightrotate = (x >> r) | (x << (32 - r));
  end
endfunction

// SHA-256 FSM 
// Get a BLOCK from the memory, COMPUTE Hash output using SHA256 function
// and write back hash value back to memory
always_ff @(posedge clk, negedge reset_n)
begin
  if (!reset_n) begin
    state <= IDLE;
  end 
  else case (state)
    // Initialize hash values h0 to h7 and a to h, other variables and memory we, address offset, etc
    IDLE: begin 
      if(start) begin
        a <= h0_i;
        b <= h1_i;
        c <= h2_i;
        d <= h3_i;
        e <= h4_i;
        f <= h5_i;
        g <= h6_i;
        h <= h7_i;
        j <= 0;

        for(index = 0; index < 16; index++)
          w[index] <= read_data[index];

        state <= COMPUTE;
      end
      else 
        state <= IDLE;
    end

    COMPUTE: begin
      // 64 processing rounds steps for 512-bit block 
      if (j < 64) begin
        {a, b, c, d, e, f, g, h} <= sha256_op(a,b,c,d,e,f,g,h,w[0],j);

        for(index = 0; index < 15; index++) 
          w[index] <= w[index + 1];
			  
        w[15] <= w_next;
        j <= j + 1;
      end
      else begin
        h0_o <= a + h0_i;
        h1_o <= b + h1_i;
        h2_o <= c + h2_i;
        h3_o <= d + h3_i;
        h4_o <= e + h4_i;
        h5_o <= f + h5_i;
        h6_o <= g + h6_i;
        h7_o <= h + h7_i;

        state <= IDLE;
      end
    end

    default: begin
      state <= IDLE;
    end
  endcase
end

// Generate done when SHA256 hash computation has finished and moved to IDLE state
assign done = (state == IDLE);

endmodule

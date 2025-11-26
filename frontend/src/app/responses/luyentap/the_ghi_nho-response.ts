export interface TheGhiNhoResponse {
  memo_id: number;
  phien_id: number;
  bo_cau_hoi: string;
  cau_hoi: string;
  dap_an_dung: 'A' | 'B' | 'C' | 'D';
  giai_thich: string | null;
  tao_luc: string; // ISO string
}

import {ChuDe} from './chude';

export interface Bocauhoi {
  id: number;
  tieu_de: string;
  mo_ta: string;
  chu_de: ChuDe;
  tao_boi_id: number;
  che_do_hien_thi: string;
  trang_thai: string;
  is_official: boolean;
  ly_do_tu_choi: string;
  is_xoa: boolean;
  so_cau_hoi: number;
}

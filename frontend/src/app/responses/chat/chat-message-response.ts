import { LoaiTinNhanChat } from '../../dtos/tinnhan/send-message-dto';

export interface ChatMessageResponse {
  tin_nhan_id: number;
  gui_boi_id: number;
  gui_boi_ten?: string;
  gui_boi_avatar?: string;
  nhan_boi_id: number;
  nhan_boi_ten?: string;
  noi_dung?: string;
  loai_tin_nhan?: LoaiTinNhanChat;
  url_media?: string;
  ten_file?: string;
  kich_thuoc_file?: number;
  gui_luc: string; // ISO
  la_toi: boolean;
}

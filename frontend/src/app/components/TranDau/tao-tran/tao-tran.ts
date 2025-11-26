import {Component, OnInit, ViewChild} from '@angular/core';
import {Base} from '../../base/base';
import {FormsModule, NgForm} from '@angular/forms';
import {CommonModule} from '@angular/common';
import {TaoTranDauDTO} from '../../../dtos/tran-dau/taotran-dto';
import Swal from 'sweetalert2';
import {ResponseObject} from '../../../responses/response-object';
import {TranDauResponse} from '../../../responses/trandau/trandau-response';
import {environment} from '../../../environments/environment';
import {PageResponse} from '../../../responses/page-response';
import {BoCauHoiResponse} from '../../../responses/bocauhoi/bocauhoi-response';

@Component({
  selector: 'app-tao-tran',
  imports: [CommonModule, FormsModule],
  templateUrl: './tao-tran.html',
  styleUrl: './tao-tran.scss',
  standalone: true
})
export class TaoTran extends Base implements OnInit {
  @ViewChild('createForm') createForm!: NgForm;   // ✅ giống style trước giờ
  saving = false;

  form: TaoTranDauDTO = new TaoTranDauDTO({});

  protected readonly environment = environment;

  isModalOpen = false;

  boCauHoiOptions: { id: number; tieu_de: string }[] = [];

  keywordBoCauHoi = '';

  // === THÊM BIẾN LƯU TRỮ METADATA ===
  preview_difficulty_counts: any = {};
  preview_type_counts: any = {};
  // === KẾT THÚC THÊM BIẾN ===

  preview_loading = false;
  preview_questions: {
    loai_noi_dung: "VAN_BAN" | "HINH_ANH" | "AM_THANH" | "VIDEO";
    do_kho: "DE" | "TRUNG_BINH" | "KHO";
    duong_dan_tep: string | null | undefined;
    noi_dung: string;
    id: number
  }[] = [];
  preview_total = 0;

  ngOnInit() {
    this.loadBoCauHoi();
  }

  onBoCauHoiChanged(bo_cau_hoi_id: number) {
    this.form.bo_cau_hoi_id = bo_cau_hoi_id;
    if (!bo_cau_hoi_id) {
      this.preview_questions = [];
      this.preview_total = 0;
      return;
    }
    this.fetchPreview(bo_cau_hoi_id);
    this.isModalOpen = true;
  }

  closeModal() {
    this.isModalOpen = false;
    this.fetchPreview(0);
  }

  fetchPreview(bo_cau_hoi_id: number) {
    console.log('Fetching preview for BoCauHoi ID:', bo_cau_hoi_id);
    this.preview_loading = true;
    this.preview_questions = [];
    this.preview_difficulty_counts = {}; // Reset thống kê
    this.preview_type_counts = {};     // Reset thống kê
    this.preview_total = 0;

    this.cauHoiService.getByBoCauHoi(bo_cau_hoi_id).subscribe({
      next: (res) => {
        const page = res.data!;
        this.preview_questions = page.items.map(q => ({
          id: q.id,
          noi_dung: q.noi_dung,
          do_kho: q.do_kho,
          loai_noi_dung: q.loai_noi_dung, // <-- THÊM DÒNG NÀY
          duong_dan_tep: q.duong_dan_tep   // <-- THÊM DÒNG NÀY
        }));
        this.preview_total = page.items.length; // tuỳ bạn đặt tên trong PageResponse
        // === TÍNH TOÁN METADATA ===
        // 2. Tính toán độ khó
        this.preview_difficulty_counts = page.items.reduce((acc, q) => {
          const key = q.do_kho; // Ví dụ: 'DE', 'KHO'
          acc[key] = (acc[key] || 0) + 1;
          return acc;
        }, {} as any); // Kết quả: { DE: 1, TRUNG_BINH: 1, KHO: 1 }

        // 3. Tính toán loại nội dung
        this.preview_type_counts = page.items.reduce((acc, q) => {
          const key = q.loai_noi_dung; // Ví dụ: 'VAN_BAN', 'HINH_ANH'
          acc[key] = (acc[key] || 0) + 1;
          return acc;
        }, {} as any); // Kết quả: { VAN_BAN: 1, HINH_ANH: 1, AM_THANH: 1 }
        // === KẾT THÚC TÍNH TOÁN ===
        this.preview_loading = false;
      },
      error: () => {
        this.preview_loading = false;
      }
    });
  }

  loadBoCauHoi() {
    this.bocauHoiService.getBattleSets().subscribe({
      next: (res: ResponseObject<PageResponse<BoCauHoiResponse>>) => {
        const page = res.data;
        this.boCauHoiOptions = page?.items ?? [];
      },
      error: () => {
        Swal.fire('Lỗi', 'Không tải được danh sách bộ câu hỏi', 'error').then(() => {
        });
      }
    });
  }


  onSubmit() {
    if (this.createForm.invalid) {
      this.createForm.control.markAllAsTouched();
      Swal.fire('Thiếu thông tin', 'Hãy kiểm tra lại các trường bắt buộc', 'info').then(r => {
      });
      return;
    }

    // validate business rule giống backend
    if (!this.form.cong_khai && (!this.form.ma_pin || !this.form.ma_pin.trim())) {
      Swal.fire('Thiếu PIN', 'Phòng riêng tư bắt buộc nhập mã PIN', 'info').then(r => {
      });
      return;
    }

    if (this.form.gioi_han_nguoi_choi < 2 || this.form.gioi_han_nguoi_choi > 4) {
      Swal.fire('Sai giới hạn người chơi', 'Giới hạn người chơi phải từ 2–4', 'info').then(r => {
      });
      return;
    }

    if (this.form.gioi_han_thoi_gian_cau_giay < 5) {
      Swal.fire('Thời gian mỗi câu quá thấp', 'Tối thiểu 5 giây mỗi câu', 'info').then(r => {
      });
      return;
    }

    this.saving = true;
    this.tranDauService.createBattle(this.form).subscribe({
      next: (res: ResponseObject<TranDauResponse>) => {
        this.saving = false;
        const data = res.data!;
        Swal.fire(
          'Tạo phòng thành công',
          `Mã phòng: ${data.ma_phong}${data.cong_khai ? '' : ' — PIN: ' + (data.ma_pin ?? '')}`,
          'success'
        ).then(() => {
          this.router.navigate(['/tran-dau/phong', data.id]).then(r => {
          });
        });
      },
      error: (e) => {
        this.saving = false;
        Swal.fire('Không thể tạo phòng', e?.error?.message || 'Thử lại sau', 'error').then(r => {
        });
      }
    });
  }

  getTieuDeBoCauHoi(): string {
    // 1. Lấy ID một cách an toàn
    const id = this.form?.bo_cau_hoi_id;

    // 2. Nếu không có ID, trả về một chuỗi mặc định
    if (id === null || id === undefined) {
      return 'Chưa chọn'; // Hoặc trả về '' (chuỗi rỗng)
    }

    // 3. Tìm tiêu đề
    const boCauHoi = this.boCauHoiOptions.find(b => b.id === id);

    // 4. Trả về tiêu đề hoặc chuỗi mặc định nếu không tìm thấy
    return boCauHoi?.tieu_de || 'Không tìm thấy';
  }

  cancel() {
    this.router.navigateByUrl('/tran-dau/pending').then(r => {
    });
  }
}

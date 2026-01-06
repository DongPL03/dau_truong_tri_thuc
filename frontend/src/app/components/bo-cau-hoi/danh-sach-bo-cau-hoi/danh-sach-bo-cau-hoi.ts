import {CommonModule} from '@angular/common';
import {Component, OnInit} from '@angular/core';
import {FormsModule} from '@angular/forms';
import Swal from 'sweetalert2';
import {ChuDe} from '../../../models/chude';
import {BoCauHoiResponse} from '../../../responses/bocauhoi/bocauhoi-response';
import {PageResponse} from '../../../responses/page-response';
import {ResponseObject} from '../../../responses/response-object';
import {Base} from '../../base/base';

@Component({
  selector: 'app-bo-cau-hoi-danh-sach-bo-cau-hoi',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './danh-sach-bo-cau-hoi.html',
  styleUrl: './danh-sach-bo-cau-hoi.scss',
})
export class BoCauHoiList extends Base implements OnInit {
  loading = false;
  keyword = '';
  cheDoHienThi = '';
  trangThai = '';
  chuDeId = 0;
  page = 0;
  limit = 20; // Tăng limit lên một chút cho đẹp grid
  sortOrder = 'NEWEST';
  totalPages = 0;
  currentUserId: number = 0;
  items: BoCauHoiResponse[] = [];
  unlocking_id: number | null = null;

  // Filter free / mất phí
  priceFilter: 'ALL' | 'FREE' | 'PAID' = 'ALL';

  // Filter rating
  minRating: number | undefined = undefined;

  chuDes: ChuDe[] = [];
  readonly trangThaiOptions = [
    {value: '', label: 'Tất cả trạng thái'},
    {value: 'DA_DUYET', label: 'Đã duyệt'},
    {value: 'CHO_DUYET', label: 'Chờ duyệt'},
    {value: 'TU_CHOI', label: 'Từ chối'},
  ];

  readonly ratingOptions = [
    {value: undefined, label: 'Tất cả đánh giá'},
    {value: 4, label: '⭐ 4+ sao'},
    {value: 3, label: '⭐ 3+ sao'},
    {value: 2, label: '⭐ 2+ sao'},
    {value: 1, label: '⭐ 1+ sao'},
  ];

  ngOnInit() {
    this.currentUserId = this.tokenService.getUserId();
    this.loadData();
    this.loadChuDe();
  }

  loadData() {
    this.loading = true;
    this.bocauHoiService
      .getAll(
        this.keyword,       // 1. keyword
        this.chuDeId,       // 2. chuDeId
        this.cheDoHienThi,  // 3. cheDoHienThi
        this.trangThai,     // 4. trangThai
        '',                 // 5. loaiSuDung (Điền rỗng nếu không dùng)
        undefined,          // 6. muonTaoTraPhi (Điền undefined để tránh lỗi boolean)
        0,                  // 7. nguoiTaoId (Điền 0 mặc định)
        this.sortOrder,     // 8. sortOrder (Giờ mới đến tham số này)
        this.page,          // 9. page
        this.limit,         // 10. limit
        this.minRating,     // 11. minRating
        undefined           // 12. maxRating
      )
      .subscribe({
        next: (res: ResponseObject<PageResponse<BoCauHoiResponse>>) => {
          const data = res.data!;
          this.items = data.items ?? [];
          this.totalPages = data.totalPages;
          this.loading = false;
        },
        error: () => {
          this.loading = false;
          Swal.fire('Lỗi', 'Không thể tải danh sách bộ câu hỏi', 'error').then((r) => {
          });
        },
      });
  }

  /** Danh sách sau khi áp dụng filter free/mất phí trên FE */
  private get baseFilteredItems(): BoCauHoiResponse[] {
    return this.items.filter((q) => {
      const isPaid = !!q.can_mo_khoa && !!q.gia_mo_khoa && q.gia_mo_khoa > 0;
      const isFree = !isPaid;

      if (this.priceFilter === 'FREE' && !isFree) {
        return false;
      }
      if (this.priceFilter === 'PAID' && !isPaid) {
        return false;
      }
      return true;
    });
  }

  // Nhóm hiển thị giống mock: bộ của tôi, bộ riêng lẻ, bộ thuộc khóa học
  get myQuizzes(): BoCauHoiResponse[] {
    return this.baseFilteredItems.filter((q) => q.nguoi_tao_id === this.currentUserId);
  }

  get standaloneQuizzes(): BoCauHoiResponse[] {
    return this.baseFilteredItems.filter(
      (q) => q.nguoi_tao_id !== this.currentUserId && !q.thuoc_khoa_hoc
    );
  }

  get courseQuizzes(): BoCauHoiResponse[] {
    return this.baseFilteredItems.filter((q) => !!q.thuoc_khoa_hoc);
  }

  loadChuDe() {
    this.chuDeService.getChuDe(0, 100).subscribe({
      next: (res: ResponseObject<any>) => {
        this.chuDes = res.data || [];
      },
      error: () => {
        console.error('Không thể tải danh sách chủ đề');
      },
    });
  }

  /** 🧠 Kiểm tra quyền sửa */
  canEdit(quiz: BoCauHoiResponse): boolean {
    return quiz.nguoi_tao_id === this.currentUserId;
  }

  /** 🧠 Kiểm tra quyền xóa */
  canDelete(quiz: BoCauHoiResponse): boolean {
    return quiz.nguoi_tao_id === this.currentUserId;
  }

  /** ⚠️ Hiện cảnh báo khi không đủ quyền */
  showAccessDeniedAlert(action: string) {
    Swal.fire({
      icon: 'warning',
      title: `Không thể ${action}`,
      text: `Bạn không thể ${action} bộ câu hỏi của người khác hoặc của admin!`,
      confirmButtonColor: '#6C5DD3',
      confirmButtonText: 'Đã hiểu',
    }).then((r) => {
    });
  }

  applyFilter() {
    this.page = 0;
    this.loadData();
  }

  highlightKeyword(text: string): string {
    if (!this.keyword) return text;
    const regex = new RegExp(`(${this.keyword})`, 'gi');
    return text.replace(regex, '<mark>$1</mark>');
  }

  clearFilter() {
    this.keyword = '';
    this.cheDoHienThi = '';
    this.trangThai = '';
    this.chuDeId = 0;
    this.minRating = undefined;
    this.priceFilter = 'ALL';
    this.loadData();
  }

  setPriceFilter(filter: 'ALL' | 'FREE' | 'PAID') {
    this.priceFilter = filter;
  }

  goToCreateQuiz() {
    this.router.navigateByUrl('/bo-cau-hoi/tao-moi-bo-cau-hoi').then((r) => {
    });
  }

  getVisiblePages(): number[] {
    const visible: number[] = [];
    const maxVisible = 5;
    const total = this.totalPages;

    if (total <= maxVisible) {
      return Array.from({length: total}, (_, i) => i);
    }

    const start = Math.max(0, this.page - 2);
    const end = Math.min(total - 1, this.page + 2);

    if (start > 0) visible.push(0);
    if (start > 1) visible.push(-1); // -1 là dấu ...

    for (let i = start; i <= end; i++) visible.push(i);

    if (end < total - 2) visible.push(-2); // -2 là dấu ...
    if (end < total - 1) visible.push(total - 1);

    return visible;
  }

  changePage(p: number) {
    if (p < 0 || p >= this.totalPages) return;
    this.page = p;
    this.loadData();
  }

  navigateDetail(id: number) {
    this.router.navigate(['/bo-cau-hoi/chi-tiet-bo-cau-hoi', id]).then((r) => {
    });
  }

  confirmDelete(id: number) {
    Swal.fire({
      title: 'Xác nhận xóa?',
      text: 'Bộ câu hỏi này và toàn bộ câu hỏi trong đó sẽ bị xóa vĩnh viễn!',
      icon: 'warning',
      showCancelButton: true,
      confirmButtonText: 'Xóa ngay',
      cancelButtonText: 'Hủy',
      confirmButtonColor: '#ef5350',
    }).then((result) => {
      if (result.isConfirmed) {
        this.deleteBoCauHoi(id);
      }
    });
  }

  deleteBoCauHoi(id: number) {
    this.bocauHoiService.delete(id).subscribe({
      next: (res) => {
        Swal.fire('Thành công', res.message || 'Xóa thành công', 'success').then((r) => {
        });
        this.loadData();
      },
      error: (err) => {
        Swal.fire('Lỗi', err.error?.message || 'Không thể xóa bộ câu hỏi', 'error').then((r) => {
        });
      },
    });
  }

  navigateEdit(id: number) {
    this.router.navigate(['/bo-cau-hoi/sua-bo-cau-hoi', id]).then((r) => {
    });
  }

  /** Click nút Luyện / Mở khoá */
  handlePracticeClick(quiz: BoCauHoiResponse) {
    if (!quiz.can_mo_khoa || quiz.da_mo_khoa) {
      this.navigatePractice(quiz.id);
      return;
    }

    const roles = this.tokenService.getRoles();
    const isAdmin = roles.includes('ROLE_ADMIN');
    if (quiz.nguoi_tao_id === this.currentUserId || isAdmin) {
      this.navigatePractice(quiz.id);
      return;
    }

    const price = quiz.gia_mo_khoa ?? 0;

    Swal.fire({
      icon: 'question',
      title: 'Mở khoá bộ câu hỏi?',
      html: `
        <p>Bộ: <strong>${quiz.tieu_de}</strong></p>
        <p>Giá mở khoá: <strong style="color: #FFC107">${price} vàng</strong></p>
      `,
      showCancelButton: true,
      confirmButtonText: 'Mở khoá',
      cancelButtonText: 'Hủy',
      confirmButtonColor: '#6C5DD3',
    }).then((result) => {
      if (result.isConfirmed) {
        this.doUnlockBoCauHoi(quiz);
      }
    });
  }

  private doUnlockBoCauHoi(quiz: BoCauHoiResponse) {
    this.unlocking_id = quiz.id;

    this.bocauHoiService.unlock_bo_cau_hoi(quiz.id).subscribe({
      next: (res: ResponseObject<any>) => {
        this.unlocking_id = null;
        const data = res.data;

        Swal.fire({
          icon: 'success',
          title: data?.da_mo_khoa_truoc_do ? 'Đã mở khoá từ trước' : 'Mở khoá thành công!',
          html: `
            <p>Bộ: <strong>${quiz.tieu_de}</strong></p>
            <p>Đã trừ: <strong>${
            data?.da_mo_khoa_truoc_do ? 0 : data?.gia_mo_khoa
          } vàng</strong></p>
            <p>Vàng còn lại: <strong>${data?.tien_vang_sau}</strong></p>
          `,
          confirmButtonText: 'Luyện ngay',
        }).then(() => {
          this.loadData();
          // this.navigatePractice(quiz.id);
        });
      },
      error: (err) => {
        this.unlocking_id = null;
        const msg = err?.error?.message || 'Không thể mở khoá bộ câu hỏi';
        Swal.fire('Lỗi', msg, 'error').then((r) => {
        });
      },
    });
  }

  private navigatePractice(id: number) {
    this.router
      .navigate(['/luyen-tap'], {
        queryParams: {bo_cau_hoi_id: id},
      })
      .then((r) => {
      });
  }
}

import {NgClass} from '@angular/common';
import {Component, OnInit, ViewChild} from '@angular/core';
import {FormsModule, NgForm} from '@angular/forms';
import Swal from 'sweetalert2';
import {ChuDe} from '../../../../models/chude';
import {ResponseObject} from '../../../../responses/response-object';
import {Base} from '../../../base/base';

@Component({
  selector: 'app-admin-bo-cau-hoi-create',
  imports: [FormsModule, NgClass],
  standalone: true,
  templateUrl: './admin-bo-cau-hoi-create.html',
  styleUrl: './admin-bo-cau-hoi-create.scss',
})
export class AdminBoCauHoiCreate extends Base implements OnInit {
  @ViewChild('createForm') createForm!: NgForm;

  loading = false;

  chu_de_list: ChuDe[] = [];

  form: any = {
    tieu_de: '',
    mo_ta: '',
    chu_de_id: 0,
    che_do_hien_thi: 'PRIVATE', // gợi ý: admin tạo bộ thi đấu → để PRIVATE
    loai_su_dung: 'RANKED_ONLY', // Mặc định cho admin: RANKED_ONLY
    muon_tao_tra_phi: false, // Admin có thể tạo trả phí hoặc miễn phí
  };

  readonly loaiSuDungOptions = [
    {value: 'RANKED_ONLY', label: 'Ranked Only (Thi đấu xếp hạng)', icon: 'fa-trophy'},
    {value: 'CASUAL_ONLY', label: 'Casual Only (Đấu vui)', icon: 'fa-gamepad'},
    {value: 'PRACTICE_ONLY', label: 'Practice Only (Chỉ luyện tập)', icon: 'fa-book'},
    {
      value: 'COURSE_ONLY',
      label: 'Course Only (Chỉ dùng cho khóa học)',
      icon: 'fa-graduation-cap',
    },
  ];

  ngOnInit(): void {
    this.loadChuDe();
  }

  loadChuDe(): void {
    this.chuDeService.getChuDe(0, 100).subscribe({
      next: (res: ResponseObject<ChuDe[]>) => {
        this.chu_de_list = res.data || [];
      },
      error: () => {
        Swal.fire('Lỗi', 'Không thể tải danh sách chủ đề', 'error').then((r) => {
        });
      },
    });
  }

  submit(): void {
    if (!this.createForm || this.createForm.invalid) {
      // Có thể báo lỗi nhẹ cho user
      Swal.fire('Thiếu dữ liệu', 'Vui lòng kiểm tra lại các trường bắt buộc', 'warning').then(
        (r) => {
        }
      );
      return;
    }

    this.loading = true;

    this.bocauHoiService.create(this.form).subscribe({
      next: (res: ResponseObject<any>) => {
        this.loading = false;
        const created = res.data;
        Swal.fire('Thành công', 'Đã tạo bộ câu hỏi mới', 'success').then(() => {
          if (created?.id) {
            this.router.navigate(['/admin/bo-cau-hoi', created.id]).then((r) => {
            });
          } else {
            this.router.navigate(['/admin/bo-cau-hoi']).then((r) => {
            });
          }
        });
      },
      error: () => {
        this.loading = false;
        Swal.fire('Lỗi', 'Không thể tạo bộ câu hỏi', 'error').then((r) => {
        });
      },
    });
  }

  cancel(): void {
    this.router.navigate(['/admin/bo-cau-hoi']);
  }

  getIconColor(type: string): string {
    switch (type) {
      case 'RANKED_ONLY':
        return 'purple';
      case 'CASUAL_ONLY':
        return 'blue';
      case 'PRACTICE_ONLY':
        return 'green';
      case 'COURSE_ONLY':
        return 'orange';
      default:
        return 'blue';
    }
  }

  getUsageDesc(type: string): string {
    switch (type) {
      case 'RANKED_ONLY':
        return 'Dùng cho thi đấu xếp hạng chính thức (Official).';
      case 'CASUAL_ONLY':
        return 'Dùng cho đấu giải trí, giao hữu không tính điểm.';
      case 'PRACTICE_ONLY':
        return 'Chỉ dùng để luyện tập cá nhân.';
      case 'COURSE_ONLY':
        return 'Dành riêng cho bài kiểm tra trong khóa học.';
      default:
        return '';
    }
  }
}

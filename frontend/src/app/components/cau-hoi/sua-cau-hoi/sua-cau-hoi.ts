import {Component, OnInit, ViewChild} from '@angular/core';
import {CommonModule} from '@angular/common';
import {FormsModule, NgForm} from '@angular/forms';
import Swal from 'sweetalert2';
import {Base} from '../../base/base';
import {ResponseObject} from '../../../responses/response-object';
import {CauHoiResponse} from '../../../responses/cauhoi/cauhoi-response';
import {CauHoiDTO} from '../../../dtos/cau-hoi/cauhoi-dto';

@Component({
  selector: 'app-sua-bo-cau-hoi-question',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './sua-cau-hoi.html',
  styleUrl: './sua-cau-hoi.scss'
})
export class CauHoiEdit extends Base implements OnInit {
  @ViewChild('form') form!: NgForm;

  questionId!: number;
  boCauHoiId!: number;
  question?: CauHoiResponse | null;

  loading = true;
  saving = false;

  // Dữ liệu hiển thị & xử lý
  previewUrl?: string;
  selectedFile?: File; // Lưu file mới nếu người dùng chọn
  readonly imageBaseUrl = 'http://localhost:8088/api/v1/cauHoi/media/';

  model: CauHoiDTO = new CauHoiDTO();
  readonly luaChonList: ('A' | 'B' | 'C' | 'D')[] = ['A', 'B', 'C', 'D'];

  ngOnInit(): void {
    this.boCauHoiId = Number(this.route.snapshot.paramMap.get('boId'));
    this.questionId = Number(this.route.snapshot.paramMap.get('id'));
    this.loadQuestion();
  }

  loadQuestion() {
    this.cauHoiService.getById(this.questionId).subscribe({
      next: (res: ResponseObject<CauHoiResponse>) => {
        this.question = res.data!;
        // Copy dữ liệu sang model
        this.model = {...this.model, ...res.data};

        // Xử lý preview URL nếu có
        if (res.data?.duong_dan_tep) {
          // Nếu đường dẫn đã chứa http thì dùng luôn, không thì nối chuỗi
          if (res.data.duong_dan_tep.startsWith('http')) {
            this.previewUrl = res.data.duong_dan_tep;
          } else {
            this.previewUrl = `${this.imageBaseUrl}${res.data.duong_dan_tep}`;
          }
        }

        this.loading = false;
      },
      error: () => {
        Swal.fire('Lỗi', 'Không thể tải dữ liệu câu hỏi', 'error')
          .then(() => this.backToBoCauHoiDetail());
        this.loading = false;
      },
    });
  }

  onLoaiNoiDungChange() {
    // Nếu chuyển sang Văn bản thì xóa file và preview
    if (this.model.loai_noi_dung === 'VAN_BAN') {
      this.previewUrl = undefined;
      this.selectedFile = undefined;
      this.model.duong_dan_tep = '';
    }
  }

  onFileSelected(event: Event) {
    const input = event.target as HTMLInputElement;
    if (!input.files?.length) return;

    const file = input.files[0];

    // Validate size (10MB)
    if (file.size > 10 * 1024 * 1024) {
      Swal.fire('File quá lớn', 'Vui lòng chọn file dưới 10MB', 'warning');
      return;
    }

    this.selectedFile = file;

    // Preview ngay lập tức (chưa upload)
    const reader = new FileReader();
    reader.onload = () => (this.previewUrl = reader.result as string);
    reader.readAsDataURL(file);
  }

  onSubmit(form: NgForm) {
    if (form.invalid) {
      Swal.fire('Thiếu thông tin', 'Vui lòng điền đầy đủ nội dung và đáp án đúng', 'warning');
      return;
    }

    this.saving = true;

    // Bước 1: Cập nhật thông tin text trước
    this.cauHoiService.update(this.questionId, this.model).subscribe({
      next: () => {
        // Bước 2: Nếu có file mới được chọn, tiến hành upload
        if (this.selectedFile && this.model.loai_noi_dung !== 'VAN_BAN') {
          this.uploadMediaAndFinish();
        } else {
          this.finishUpdate();
        }
      },
      error: (err) => {
        this.saving = false;
        Swal.fire('Lỗi', err.error?.message || 'Cập nhật thất bại', 'error');
      }
    });
  }

  uploadMediaAndFinish() {
    if (!this.selectedFile) return;

    // Lưu ý: service uploadMedia cần ID câu hỏi
    const loai = this.model.loai_noi_dung as 'HINH_ANH' | 'AM_THANH';

    this.cauHoiService.uploadMedia(this.questionId, this.selectedFile, loai).subscribe({
      next: (res) => {
        this.model.duong_dan_tep = res.data; // Cập nhật lại đường dẫn mới trả về
        this.finishUpdate();
      },
      error: () => {
        this.saving = false;
        Swal.fire('Cảnh báo', 'Cập nhật thông tin thành công nhưng lỗi tải file', 'warning')
          .then(() => this.backToBoCauHoiDetail());
      }
    });
  }

  finishUpdate() {
    this.saving = false;
    Swal.fire({
      icon: 'success',
      title: 'Thành công',
      text: 'Đã cập nhật câu hỏi!',
      timer: 1500,
      showConfirmButton: false
    }).then(() => this.backToBoCauHoiDetail());
  }

  handleImgError(event: any) {
    // Fallback nếu ảnh lỗi
    event.target.src = 'assets/images/image-placeholder.png';
  }

  backToBoCauHoiDetail() {
    this.router.navigateByUrl(`/bo-cau-hoi/chi-tiet-bo-cau-hoi/${this.boCauHoiId}`);
  }
}
